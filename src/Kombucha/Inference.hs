module Kombucha.Inference
  ( Env (..),
    TypeError (..),
    checkClaim,
    checkDocument,
    emptyEnv,
    exprType,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Control.Monad.Trans.State as State
import Control.Monad.Trans.Writer
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Kombucha.SyntaxTree
import qualified Kombucha.TwoOrMore as TwoOrMore
import Prettyprinter

newtype Infer a = Infer (WriterT [Constraint] (StateT InferState (Except TypeError)) a)
  deriving (Applicative, Functor, Monad)

data Constraint = Type :~ Type
  deriving (Show)

data InferState = InferState {count :: Int, env :: Env}

data Env = Env
  { types :: Map Name TypeDeclaration,
    terms :: Map Name Scheme
  }

instance Pretty Env where
  pretty env =
    blankLineSep (map (pretty . snd) $ Map.toList $ types env)
      <> line
      <> line
      <> blankLineSep (map prettyTerm $ Map.toList $ terms env)
    where
      blankLineSep = concatWith $ \x y -> x <> line <> line <> y
      prettyTerm (name, scheme) = pretty name <> ":" <> nest 4 (line <> pretty scheme)

data TypeError
  = AlreadyBound Name
  | ArityMismatch [Type] [Type]
  | InfiniteType Name Type
  | TypeMismatch Type Type
  | UnboundVariable Name
  | UnusedVariables [Name]
  deriving (Eq, Show)

type Substitution = Map Name Type

type Solve = ReaderT (Set Name) (Except TypeError)

class Substitutable a where
  apply :: Substitution -> a -> a
  freeVars :: a -> Set Name

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  freeVars = foldr (Set.union . freeVars) Set.empty

instance Substitutable t => Substitutable (Qualified t) where
  apply subst (predicates :=> type') = map (apply subst) predicates :=> apply subst type'
  freeVars (predicates :=> type') = freeVars predicates `Set.union` freeVars type'

instance Substitutable Constraint where
  apply subst (type1 :~ type2) = apply subst type1 :~ apply subst type2
  freeVars (type1 :~ type2) = freeVars type1 `Set.union` freeVars type2

instance Substitutable Predicate where
  apply subst (IsResource type') = IsResource $ apply subst type'
  apply subst (IsParam type' name) = IsParam (apply subst type') name

  freeVars (IsResource type') = freeVars type'
  freeVars (IsParam type' _) = freeVars type'

instance Substitutable Type where
  apply subst (TypeInference inference) = TypeInference $ apply subst inference
  apply subst (TypeResource resource) = TypeResource $ apply subst resource
  apply _ (TypeParam param) = TypeParam param
  apply subst var@(TypeVariable name) = Map.findWithDefault var name subst

  freeVars (TypeInference inference) = freeVars inference
  freeVars (TypeResource resource) = freeVars resource
  freeVars (TypeParam _) = Set.empty
  freeVars (TypeVariable name) = Set.singleton name

instance Substitutable Inference where
  apply subst (resource1 :|- resource2) = apply subst resource1 :|- apply subst resource2
  freeVars (resource1 :|- resource2) = freeVars resource1 `Set.union` freeVars resource2

instance Substitutable Resource where
  apply _ ResourceUnit = ResourceUnit
  apply subst (ResourceAtom name params) = ResourceAtom name $ apply subst <$> params
  apply subst (ResourceTuple resources) = ResourceTuple $ apply subst <$> resources

  freeVars ResourceUnit = Set.empty
  freeVars (ResourceAtom _ params) = freeVars params
  freeVars (ResourceTuple resources) = freeVars $ TwoOrMore.toList resources

-- * Typing environment

emptyEnv :: Env
emptyEnv = Env {types = Map.empty, terms = Map.empty}

insertType :: Name -> TypeDeclaration -> Env -> Env
insertType name declaration env = env {types = Map.insert name declaration $ types env}

insertTerm :: Name -> Scheme -> Env -> Env
insertTerm name scheme env = env {terms = Map.insert name scheme $ terms env}

deleteTerm :: Name -> Env -> Env
deleteTerm name env = env {terms = Map.delete name $ terms env}

-- * Type checking and inference

checkDocument :: Document -> Either TypeError Env
checkDocument = foldM foldDeclarations emptyEnv
  where
    foldDeclarations env declaration =
      case declaration of
        DeclareType param@(DeclareParam ParamSpec {name}) ->
          return $ insertType name param env
        DeclareType resource@(DeclareResource ResourceSpec {name}) ->
          return $ insertType name resource env
        DeclareAxiom Axiom {name, inference} -> do
          scheme <- inferenceScheme env inference
          return $ insertTerm name scheme env
        DeclareClaim claim@Claim {name, inference} -> do
          claimPreds <- checkClaim env claim
          ForAll vars (inferencePreds :=> type') <- inferenceScheme env inference
          let scheme' = ForAll vars $ nub (claimPreds ++ inferencePreds) :=> type'
          return $ insertTerm name scheme' env

inferenceScheme :: Env -> Inference -> Either TypeError Scheme
inferenceScheme env inference = do
  predicates <- nub <$> typePredicates env (TypeInference inference)
  return $ ForAll (freeVars inference) $ predicates :=> TypeInference inference

typePredicates :: Env -> Type -> Either TypeError [Predicate]
typePredicates env type' = case type' of
  TypeInference (lhs :|- rhs) -> do
    lhsPreds <- typePredicates env lhs
    rhsPreds <- typePredicates env rhs
    return $ lhsPreds ++ rhsPreds
  TypeResource ResourceUnit -> return []
  TypeResource (ResourceAtom name givenParams) ->
    case Map.lookup name $ types env of
      Just (DeclareResource ResourceSpec {params = expectedParams})
        | length givenParams == length expectedParams ->
          return $ zipWith IsParam givenParams expectedParams
      _ -> Left $ UnboundVariable name
  TypeResource (ResourceTuple resources) -> do
    predicates <- mapM (typePredicates env) $ TwoOrMore.toList resources
    return $ concat predicates
  TypeParam _ -> return []
  TypeVariable _ -> return [IsResource type']

checkClaim :: Env -> Claim -> Either TypeError [Predicate]
checkClaim env claim@Claim {inference} = do
  (proofPreds :=> _, constraints) <- runInfer env (inferClaim claim)
  inferencePreds <- typePredicates env $ TypeInference inference
  let predicates = nub $ proofPreds ++ inferencePreds

  subst <- runSolve (freeVars inference) constraints
  return $ apply subst predicates

exprType :: Env -> Expr -> Either TypeError (Qualified Type)
exprType env expr = do
  (type', constraints) <- runInfer env $ inferExpr expr
  subst <- runSolve Set.empty constraints
  return $ apply subst type'

runInfer :: Env -> Infer a -> Either TypeError (a, [Constraint])
runInfer env (Infer m) = runExcept $ evalStateT (runWriterT m) $ InferState {count = 0, env}

inferClaim :: Claim -> Infer (Qualified Type)
inferClaim Claim {inference = lhs :|- rhs, proof = input `Proves` output} =
  checkEnv $ do
    inputPreds :=> inputType <- inferPattern input
    outputPreds :=> outputType <- inferExpr output

    constrain $ lhs :~ inputType
    constrain $ rhs :~ outputType

    let predicates = inputPreds ++ outputPreds
    return $ predicates :=> TypeInference (inputType :|- outputType)

inferExpr :: Expr -> Infer (Qualified Type)
inferExpr ExprUnit = return $ [] :=> TypeResource ResourceUnit
inferExpr (ExprVariable name) = do
  type' <- lookupTerm name
  state <- getState
  putState state {env = deleteTerm name $ env state}
  return type'
inferExpr (ExprTuple exprs) = do
  (predicates, types) <- extractPredicates <$> mapM inferExpr exprs
  return $ predicates :=> TypeResource (ResourceTuple $ TwoOrMore.fromList types)
inferExpr (ExprLet binding value) = do
  rhsPreds :=> rhs <- inferExpr value
  lhsPreds :=> lhs <- inferPattern binding
  constrain $ lhs :~ rhs
  return $ (lhsPreds ++ rhsPreds) :=> TypeResource ResourceUnit
inferExpr (ExprApply name arg) = do
  namePreds :=> nameType <- lookupTerm name
  argPreds :=> argType <- inferExpr arg
  resultType <- fresh
  constrain $ nameType :~ TypeInference (argType :|- resultType)
  return $ (namePreds ++ argPreds) :=> resultType
inferExpr (ExprBlock exprs) =
  checkEnv $
    foldM (const inferExpr) ([] :=> TypeResource ResourceUnit) exprs

inferPattern :: Pattern -> Infer (Qualified Type)
inferPattern PatternUnit = return $ [] :=> TypeResource ResourceUnit
inferPattern (PatternBind name) = do
  before <- getState
  when (name `Map.member` terms (env before)) $ inferError $ AlreadyBound name
  var <- fresh
  let var' = [] :=> var

  modifyState $ \state ->
    state {env = insertTerm name (ForAll Set.empty var') $ env state}

  return var'
inferPattern (PatternTuple patterns) = do
  (predicates, types) <- extractPredicates <$> mapM inferPattern patterns
  return $ predicates :=> TypeResource (ResourceTuple $ TwoOrMore.fromList types)

extractPredicates :: Foldable t => t (Qualified a) -> ([Predicate], [a])
extractPredicates =
  foldr
    (\(predicates' :=> type') (predicates, types) -> (predicates ++ predicates', type' : types))
    ([], [])

checkEnv :: Infer a -> Infer a
checkEnv infer = do
  before <- getState
  result <- infer
  after <- getState

  let unusedVars = terms (env after) `Map.difference` terms (env before)
  unless (null unusedVars) $ inferError $ UnusedVariables $ Map.keys unusedVars

  return result

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Type
fresh = do
  state <- getState
  putState state {count = count state + 1}
  return $ TypeVariable $ letters !! count state

instantiate :: Scheme -> Infer (Qualified Type)
instantiate (ForAll vars type') = do
  varList' <- forM varList $ const fresh
  let subst = Map.fromList $ zip varList varList'
  return $ apply subst type'
  where
    varList = Set.toList vars

lookupTerm :: Name -> Infer (Qualified Type)
lookupTerm name = do
  InferState {env} <- getState
  case Map.lookup name $ terms env of
    Just scheme -> instantiate scheme
    Nothing -> inferError $ UnboundVariable name

inferError :: TypeError -> Infer a
inferError = Infer . lift . lift . throwE

getState :: Infer InferState
getState = Infer . lift $ State.get

putState :: InferState -> Infer ()
putState = Infer . lift . State.put

modifyState :: (InferState -> InferState) -> Infer ()
modifyState = Infer . lift . State.modify

constrain :: Constraint -> Infer ()
constrain constraint@(type1 :~ type2)
  | type1 /= type2 = Infer $ tell [constraint]
  | otherwise = return ()

-- * Type unification and constraint solving

unify :: Type -> Type -> Solve Substitution
unify type1 type2
  | type1 == type2 = return Map.empty
  | otherwise = do
    rigidVars <- ask
    let free var = not $ var `Set.member` rigidVars

    case (type1, type2) of
      (TypeInference (in1 :|- out1), TypeInference (in2 :|- out2)) ->
        unifyZip [in1, out1] [in2, out2]
      (TypeResource resource1, TypeResource resource2) ->
        unifyResource resource1 resource2
      (TypeVariable var, type')
        | free var -> var `bind` type'
      (type', TypeVariable var)
        | free var -> var `bind` type'
      _ -> solveError $ TypeMismatch type1 type2

unifyResource :: Resource -> Resource -> Solve Substitution
unifyResource resource1 resource2
  | resource1 == resource2 = return Map.empty
  | otherwise =
    case (resource1, resource2) of
      (ResourceAtom name1 params1, ResourceAtom name2 params2)
        | name1 == name2 -> unifyZip params1 params2
      (ResourceTuple resources1, ResourceTuple resources2) ->
        unifyZip (TwoOrMore.toList resources1) (TwoOrMore.toList resources2)
      _ -> solveError $ TypeMismatch (TypeResource resource1) (TypeResource resource2)

unifyZip :: [Type] -> [Type] -> Solve Substitution
unifyZip [] [] = return Map.empty
unifyZip (type1 : types1) (type2 : types2) = do
  subst1 <- unify type1 type2
  subst2 <- unifyZip (apply subst1 types1) (apply subst1 types2)
  return $ subst2 `compose` subst1
unifyZip type1 type2 = solveError $ ArityMismatch type1 type2

runSolve :: Set Name -> [Constraint] -> Either TypeError Substitution
runSolve rigidVars constraints = runExcept $ runReaderT (solve Map.empty constraints) rigidVars

solve :: Substitution -> [Constraint] -> Solve Substitution
solve subst [] = return subst
solve subst (type1 :~ type2 : constraints) = do
  subst' <- unify type1 type2
  solve (subst' `compose` subst) (apply subst' constraints)

compose :: Substitution -> Substitution -> Substitution
subst1 `compose` subst2 = Map.map (apply subst1) subst2 `Map.union` subst1

bind :: Name -> Type -> Solve Substitution
name `bind` type'
  | type' == TypeVariable name = return Map.empty
  | occursCheck name type' = solveError $ InfiniteType name type'
  | otherwise = do
    rigidVars <- ask
    if name `Set.member` rigidVars
      then error "Cannot bind a rigid type variable."
      else return $ Map.singleton name type'

occursCheck :: Substitutable a => Name -> a -> Bool
occursCheck name x = name `Set.member` freeVars x

solveError :: TypeError -> Solve a
solveError = lift . throwE
