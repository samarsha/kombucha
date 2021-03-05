module Kombucha.Inference (Infer, inferExpr, runInfer) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Kombucha.SyntaxTree

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
  | UnboundVariable Name
  deriving (Show)

type Subst = Map Name Type

class Substitutable a where
  apply :: Subst -> a -> a

instance Substitutable Type where
  apply subst (TypeInference inference) = TypeInference $ apply subst inference
  apply subst (TypeResource resource) = TypeResource $ apply subst resource
  apply subst (TypeParam param) = TypeParam $ apply subst param
  apply subst t@(TypeVariable name) = Map.findWithDefault t name subst

instance Substitutable Inference where
  apply subst (lhs `Infers` rhs) = apply subst lhs `Infers` apply subst rhs

instance Substitutable Resource where
  apply _ ResourceUnit = ResourceUnit
  apply subst (ResourceAtom name params) = ResourceAtom name $ apply subst <$> params
  apply subst (ResourceTuple resources) = ResourceTuple $ apply subst <$> resources
  apply subst resource@(ResourceVariable name) =
    case Map.lookup name subst of
      Just (TypeResource resource') -> resource'
      _ -> resource

instance Substitutable Param where
  apply _ param@(ParamValue _) = param
  apply subst param@(ParamVariable name) =
    case Map.lookup name subst of
      Just (TypeParam param') -> param'
      _ -> param

runInfer :: Env -> Infer a -> Either TypeError (a, [Constraint])
runInfer env (Infer m) = runExcept $ evalStateT (runWriterT $ runReaderT m env) $ InferState {count = 0}

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
  valueType <- ResourceVariable <$> fresh
  constrain $ nameType :~ TypeInference (argType `Infers` valueType)
  passEnv $ TypeResource valueType
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

instantiate :: Scheme Type -> Infer Type
instantiate (ForAll variables t) = do
  variables' <- mapM (const $ TypeVariable <$> fresh) variables
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
constrain c@(lhs :~ rhs)
  | lhs /= rhs = Infer . lift $ tell [c]
  | otherwise = return ()
