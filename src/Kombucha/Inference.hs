module Kombucha.Inference
  ( Env,
    TypeError (..),
    checkClaim,
    typeExpr,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Kombucha.SyntaxTree
import qualified Kombucha.TwoOrMore as TwoOrMore

newtype Infer a
  = Infer
      ( ReaderT Env (WriterT [Constraint] (StateT InferState (Except TypeError))) a
      )
  deriving (Applicative, Functor, Monad)

newtype InferState = InferState {count :: Int}

data Constraint = Type :~ Type
  deriving (Show)

type Env = Map Name (Scheme Type)

data TypeError
  = TypeMismatch Type Type
  | ArityMismatch [Type] [Type]
  | InfiniteType Name Type
  | ConstrainsVariable Name Type
  | UnboundVariable Name
  deriving (Eq, Show)

type Subst = Map Name Type

type Solve = Except TypeError

class Substitutable a where
  apply :: Subst -> a -> a
  freeVariables :: a -> Set Name

instance Substitutable a => Substitutable [a] where
  apply = map . apply
  freeVariables = foldr (Set.union . freeVariables) Set.empty

instance Substitutable Constraint where
  apply subst (t1 :~ t2) = apply subst t1 :~ apply subst t2
  freeVariables (t1 :~ t2) = freeVariables t1 `Set.union` freeVariables t2

instance Substitutable Type where
  apply subst (TypeInference inference) = TypeInference $ apply subst inference
  apply subst (TypeResource resource) = TypeResource $ apply subst resource
  apply subst (TypeParam param) = TypeParam $ apply subst param
  apply subst t@(TypeVariable name) = Map.findWithDefault t name subst

  freeVariables (TypeInference inference) = freeVariables inference
  freeVariables (TypeResource resource) = freeVariables resource
  freeVariables (TypeParam param) = freeVariables param
  freeVariables (TypeVariable name) = Set.singleton name

instance Substitutable Inference where
  apply subst (r1 `Infers` r2) = apply subst r1 `Infers` apply subst r2
  freeVariables (r1 `Infers` r2) = freeVariables r1 `Set.union` freeVariables r2

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

  freeVariables ResourceUnit = Set.empty
  freeVariables (ResourceAtom _ params) = freeVariables params
  freeVariables (ResourceTuple rs) = freeVariables $ TwoOrMore.toList rs
  freeVariables (ResourceVariable name) = Set.singleton name

instance Substitutable Param where
  apply _ param@(ParamValue _) = param
  apply subst param@(ParamVariable name) =
    case Map.lookup name subst of
      Just (TypeVariable name') -> ParamVariable name'
      Just (TypeParam param') -> param'
      Just _ -> error "Type kind can't be substituted here."
      Nothing -> param

  freeVariables (ParamValue _) = Set.empty
  freeVariables (ParamVariable name) = Set.singleton name

checkClaim :: Env -> Claim -> Either TypeError ()
checkClaim env claim = do
  -- TODO: Check that claim doesn't constrain type variables.
  (_, constraints) <- runInfer env $ inferClaim claim
  void $ runSolve constraints

typeExpr :: Env -> Expr -> Either TypeError Type
typeExpr env expr = do
  ((t, _), constraints) <- runInfer env $ inferExpr expr
  subst <- runSolve constraints
  return $ apply subst t

runInfer :: Env -> Infer a -> Either TypeError (a, [Constraint])
runInfer env (Infer m) =
  runExcept $
    evalStateT (runWriterT $ runReaderT m env) $
      InferState {count = 0}

inferClaim :: Claim -> Infer Inference
inferClaim Claim {inference = scheme, proof = pattern `Proves` expr} = do
  (patternResource, env) <- inferPattern pattern
  exprType <- fst <$> withEnv env (inferExpr expr)

  let patternType = TypeResource patternResource
  constrain $ patternType :~ exprType

  inference@(input `Infers` output) <- instantiate scheme
  constrain $ TypeResource input :~ patternType
  constrain $ TypeResource output :~ exprType
  return inference

inferExpr :: Expr -> Infer (Type, Env)
inferExpr ExprUnit = passEnv $ TypeResource ResourceUnit
inferExpr (ExprVariable name) = lookupEnv name >>= passEnv
inferExpr (ExprTuple exprs) = do
  types <- fmap fst <$> mapM inferExpr exprs
  resources <- mapM resourceType types
  passEnv $ TypeResource $ ResourceTuple resources
inferExpr (ExprLet pattern expr) = do
  (resource, env) <- inferPattern pattern
  exprType <- fst <$> inferExpr expr
  constrain $ TypeResource resource :~ exprType
  return (TypeResource ResourceUnit, env)
inferExpr (ExprApply name arg) = do
  nameType <- lookupEnv name
  argType <- inferExpr arg >>= resourceType . fst
  resultType <- ResourceVariable <$> fresh
  constrain $ nameType :~ TypeInference (argType `Infers` resultType)
  passEnv $ TypeResource resultType
inferExpr (ExprBlock exprs) = do
  env <- getEnv
  env' <- foldM foldBlock env $ NonEmpty.init exprs
  t <- fmap fst $ withEnv env' $ inferExpr $ NonEmpty.last exprs
  return (t, env)
  where
    foldBlock env blockExpr = do
      (t, env') <- withEnv env $ inferExpr blockExpr
      constrain $ TypeResource ResourceUnit :~ t
      return env'

inferPattern :: Pattern -> Infer (Resource, Env)
inferPattern PatternUnit = passEnv ResourceUnit
inferPattern (PatternBind name) = do
  env <- getEnv
  rv <- ResourceVariable <$> fresh
  return (rv, Map.insert name (ForAll [] $ TypeResource rv) env)
inferPattern (PatternTuple patterns) = do
  results <- mapM inferPattern patterns
  let resources = fst <$> results
  let env = foldl1 Map.union $ snd <$> results
  return (ResourceTuple resources, env)

letters :: [String]
letters = [1 ..] >>= flip replicateM ['a' .. 'z']

fresh :: Infer Name
fresh = do
  s <- getState
  putState s {count = count s + 1}
  return $ letters !! count s

instantiate :: Substitutable a => Scheme a -> Infer a
instantiate (ForAll variables t) = do
  variables' <- forM variables $ const $ TypeVariable <$> fresh
  let subst = Map.fromList $ zip variables variables'
  return $ apply subst t

resourceType :: Type -> Infer Resource
resourceType t = do
  rv <- ResourceVariable <$> fresh
  constrain $ TypeResource rv :~ t
  return rv

getEnv :: Infer Env
getEnv = Infer ask

withEnv :: Env -> Infer a -> Infer a
withEnv env (Infer m) = Infer $ local (const env) m

passEnv :: a -> Infer (a, Env)
passEnv x = do
  env <- getEnv
  return (x, env)

lookupEnv :: Name -> Infer Type
lookupEnv name = do
  env <- getEnv
  case Map.lookup name env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ UnboundVariable name

throwError :: TypeError -> Infer a
throwError = Infer . lift . lift . lift . throwE

getState :: Infer InferState
getState = Infer . lift $ lift get

putState :: InferState -> Infer ()
putState = Infer . lift . lift . put

constrain :: Constraint -> Infer ()
constrain c@(t1 :~ t2)
  | t1 /= t2 = Infer . lift $ tell [c]
  | otherwise = return ()

unify :: Type -> Type -> Solve Subst
unify t1 t2 | t1 == t2 = return Map.empty
unify (TypeInference (r1 `Infers` r2)) (TypeInference (r3 `Infers` r4)) =
  unifyZip [TypeResource r1, TypeResource r2] [TypeResource r3, TypeResource r4]
unify (TypeResource (ResourceAtom name1 params1)) (TypeResource (ResourceAtom name2 params2))
  | name1 == name2 = unifyZip (TypeParam <$> params1) (TypeParam <$> params2)
unify (TypeResource (ResourceTuple rs1)) (TypeResource (ResourceTuple rs2)) =
  unifyZip (TypeResource <$> TwoOrMore.toList rs1) (TypeResource <$> TwoOrMore.toList rs2)
unify (TypeResource (ResourceVariable rv)) t@(TypeResource _) = rv `bind` t
unify t@(TypeResource _) (TypeResource (ResourceVariable rv)) = rv `bind` t
unify (TypeParam (ParamVariable pv)) t@(TypeParam _) = pv `bind` t
unify t@(TypeParam _) (TypeParam (ParamVariable pv)) = pv `bind` t
unify (TypeVariable tv) t = tv `bind` t
unify t (TypeVariable tv) = tv `bind` t
unify t1 t2 = throwE $ TypeMismatch t1 t2

unifyZip :: [Type] -> [Type] -> Solve Subst
unifyZip [] [] = return Map.empty
unifyZip (t1 : ts1) (t2 : ts2) = do
  subst1 <- unify t1 t2
  subst2 <- unifyZip (apply subst1 ts1) (apply subst1 ts2)
  return $ subst2 `compose` subst1
unifyZip t1 t2 = throwE $ ArityMismatch t1 t2

runSolve :: [Constraint] -> Either TypeError Subst
runSolve constraints = runExcept $ solve (Map.empty, constraints)

solve :: (Subst, [Constraint]) -> Solve Subst
solve (subst, []) = return subst
solve (subst, t1 :~ t2 : constraints) = do
  subst' <- unify t1 t2
  solve (subst' `compose` subst, apply subst' constraints)

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1

bind :: Name -> Type -> Solve Subst
name `bind` t
  | t == TypeVariable name = return Map.empty
  | t == TypeResource (ResourceVariable name) = return Map.empty
  | t == TypeParam (ParamVariable name) = return Map.empty
  | occursCheck name t = throwE $ InfiniteType name t
  | otherwise = return $ Map.singleton name t

occursCheck :: Substitutable a => Name -> a -> Bool
occursCheck name s = name `Set.member` freeVariables s
