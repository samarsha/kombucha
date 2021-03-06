module Kombucha.Inference
  ( Env,
    TypeError (..),
    checkClaim,
    checkDocument,
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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Kombucha.SyntaxTree
import qualified Kombucha.TwoOrMore as TwoOrMore

newtype Infer a = Infer (WriterT [Constraint] (StateT InferState (Except TypeError)) a)
  deriving (Applicative, Functor, Monad)

data InferState = InferState {count :: Int, env :: Env}

data Constraint = Type :~ Type
  deriving (Show)

type Env = Map Name (Scheme Type)

data TypeError
  = ArityMismatch [Type] [Type]
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

instance Substitutable Constraint where
  apply subst (type1 :~ type2) = apply subst type1 :~ apply subst type2
  freeVars (type1 :~ type2) = freeVars type1 `Set.union` freeVars type2

instance Substitutable Type where
  apply subst (TypeInference inference) = TypeInference $ apply subst inference
  apply subst (TypeResource resource) = TypeResource $ apply subst resource
  apply subst (TypeParam param) = TypeParam $ apply subst param
  apply subst var@(TypeVariable name) = Map.findWithDefault var name subst

  freeVars (TypeInference inference) = freeVars inference
  freeVars (TypeResource resource) = freeVars resource
  freeVars (TypeParam param) = freeVars param
  freeVars (TypeVariable name) = Set.singleton name

instance Substitutable Inference where
  apply subst (resource1 :|- resource2) = apply subst resource1 :|- apply subst resource2
  freeVars (resource1 :|- resource2) = freeVars resource1 `Set.union` freeVars resource2

instance Substitutable Resource where
  apply _ ResourceUnit = ResourceUnit
  apply subst (ResourceAtom name params) = ResourceAtom name $ apply subst <$> params
  apply subst (ResourceTuple resources) = ResourceTuple $ apply subst <$> resources
  apply subst resource@(ResourceVariable name) =
    case Map.lookup name subst of
      Just (TypeVariable name') -> ResourceVariable name'
      Just (TypeResource resource') -> resource'
      Just _ -> error "Type kind can't be substituted here."
      Nothing -> resource

  freeVars ResourceUnit = Set.empty
  freeVars (ResourceAtom _ params) = freeVars params
  freeVars (ResourceTuple resources) = freeVars $ TwoOrMore.toList resources
  freeVars (ResourceVariable name) = Set.singleton name

instance Substitutable Param where
  apply _ param@(ParamValue _) = param
  apply subst param@(ParamVariable name) =
    case Map.lookup name subst of
      Just (TypeVariable name') -> ParamVariable name'
      Just (TypeParam param') -> param'
      Just _ -> error "Type kind can't be substituted here."
      Nothing -> param

  freeVars (ParamValue _) = Set.empty
  freeVars (ParamVariable name) = Set.singleton name

checkDocument :: Document -> Either TypeError ()
checkDocument = flip foldM_ Map.empty $ \env declaration ->
  case declaration of
    DeclareAxiom Axiom {name, inference} ->
      Right $ Map.insert name (TypeInference <$> inference) env
    DeclareClaim claim@Claim {name, inference} -> do
      checkClaim env claim
      Right $ Map.insert name (TypeInference <$> inference) env
    _ -> error "Unsupported declaration."

checkClaim :: Env -> Claim -> Either TypeError ()
checkClaim env claim@Claim {inference = ForAll rigidVars _} = do
  constraints <- snd <$> runInfer env (inferClaim claim)
  void $ runSolve (Set.fromList rigidVars) constraints

exprType :: Env -> Expr -> Either TypeError Type
exprType env expr = do
  (type', constraints) <- runInfer env $ inferExpr expr
  subst <- runSolve Set.empty constraints
  return $ apply subst type'

runInfer :: Env -> Infer a -> Either TypeError (a, [Constraint])
runInfer env (Infer m) = runExcept $ evalStateT (runWriterT m) $ InferState {count = 0, env}

inferClaim :: Claim -> Infer ()
inferClaim
  Claim
    { inference = ForAll _ (lhs :|- rhs),
      proof = input `Proves` output
    } =
    restoreEnv $ do
      inputResource <- inferPattern input
      outputType <- inferExpr output

      let inputType = TypeResource inputResource
      constrain $ TypeResource lhs :~ inputType
      constrain $ TypeResource rhs :~ outputType

inferExpr :: Expr -> Infer Type
inferExpr ExprUnit = return $ TypeResource ResourceUnit
inferExpr (ExprVariable name) = do
  type' <- lookupEnv name
  state <- getState
  putState state {env = Map.delete name $ env state}
  return type'
inferExpr (ExprTuple exprs) = do
  types <- mapM inferExpr exprs
  resources <- mapM resourceType types
  return $ TypeResource $ ResourceTuple resources
inferExpr (ExprLet binding value) = do
  lhs <- inferPattern binding
  rhs <- inferExpr value
  constrain $ TypeResource lhs :~ rhs
  return $ TypeResource ResourceUnit
inferExpr (ExprApply name arg) = do
  nameType <- lookupEnv name
  argType <- inferExpr arg >>= resourceType
  resultType <- ResourceVariable <$> fresh
  constrain $ nameType :~ TypeInference (argType :|- resultType)
  return $ TypeResource resultType
inferExpr (ExprBlock exprs) = restoreEnv $ foldM (const inferExpr) (TypeResource ResourceUnit) exprs

inferPattern :: Pattern -> Infer Resource
inferPattern PatternUnit = return ResourceUnit
inferPattern (PatternBind name) = do
  -- TODO: Disallow shadowing?
  var <- ResourceVariable <$> fresh
  state <- getState
  putState state {env = Map.insert name (ForAll [] $ TypeResource var) $ env state}
  return var
inferPattern (PatternTuple patterns) = ResourceTuple <$> mapM inferPattern patterns

restoreEnv :: Infer a -> Infer a
restoreEnv infer = do
  before <- getState
  result <- infer
  after <- getState

  let unusedVars = env after `Map.difference` env before
  unless (null unusedVars) $ inferError $ UnusedVariables $ Map.keys unusedVars
  modifyState $ \state -> state {env = env before}

  return result

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Name
fresh = do
  state <- getState
  putState state {count = count state + 1}
  return $ letters !! count state

instantiate :: Substitutable a => Scheme a -> Infer a
instantiate (ForAll vars type') = do
  vars' <- forM vars $ const $ TypeVariable <$> fresh
  let subst = Map.fromList $ zip vars vars'
  return $ apply subst type'

resourceType :: Type -> Infer Resource
resourceType type' = do
  var <- ResourceVariable <$> fresh
  constrain $ TypeResource var :~ type'
  return var

lookupEnv :: Name -> Infer Type
lookupEnv name = do
  InferState {env} <- getState
  case Map.lookup name env of
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

unify :: Type -> Type -> Solve Substitution
unify type1 type2
  | type1 == type2 = return Map.empty
  | otherwise = do
    rigidVars <- ask
    let free var = not $ var `Set.member` rigidVars

    case (type1, type2) of
      (TypeInference (in1 :|- out1), TypeInference (in2 :|- out2)) ->
        unifyZip
          [TypeResource in1, TypeResource out1]
          [TypeResource in2, TypeResource out2]
      (TypeResource resource1, TypeResource resource2) ->
        unifyResource resource1 resource2
      (TypeParam (ParamVariable var), param@(TypeParam _))
        | free var -> var `bind` param
      (param@(TypeParam _), TypeParam (ParamVariable var))
        | free var -> var `bind` param
      (TypeVariable var, type')
        | free var -> var `bind` type'
      (type', TypeVariable var)
        | free var -> var `bind` type'
      _ -> solveError $ TypeMismatch type1 type2

unifyResource :: Resource -> Resource -> Solve Substitution
unifyResource resource1 resource2
  | resource1 == resource2 = return Map.empty
  | otherwise = do
    rigidVars <- ask
    let free var = not $ var `Set.member` rigidVars

    case (resource1, resource2) of
      (ResourceAtom name1 params1, ResourceAtom name2 params2)
        | name1 == name2 -> unifyZip (TypeParam <$> params1) (TypeParam <$> params2)
      (ResourceTuple resources1, ResourceTuple resources2) ->
        unifyZip
          (TypeResource <$> TwoOrMore.toList resources1)
          (TypeResource <$> TwoOrMore.toList resources2)
      (ResourceVariable var, resource)
        | free var -> var `bind` TypeResource resource
      (resource, ResourceVariable var)
        | free var -> var `bind` TypeResource resource
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
  | type' == TypeResource (ResourceVariable name) = return Map.empty
  | type' == TypeParam (ParamVariable name) = return Map.empty
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
