module Kombucha.Inference (inferExpr, runInfer) where

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

type Infer a =
  ReaderT
    Env
    ( WriterT
        [Constraint]
        ( StateT
            InferState
            (Except TypeError)
        )
    )
    a

newtype InferState = InferState {count :: Int}

data Constraint = Type :~ Type

type Env = Map Name (Scheme Type)

data TypeError
  = UnificationFail Type Type
  | UnboundVariable Name
  | TypeKindMismatch Type TypeKind

data TypeKind
  = KindInference
  | KindResource
  | KindParam

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

runInfer :: Env -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalStateT (runWriterT $ runReaderT m env) $ InferState {count = 0}

inferExpr :: Expr -> Infer (Type, Env)
inferExpr expr = case expr of
  ExprUnit -> preserveEnv $ TypeResource ResourceUnit
  ExprVariable name -> lookupEnv name >>= preserveEnv
  ExprTuple exprs -> do
    types <- fmap fst <$> mapM inferExpr exprs
    resources <- mapM resourceType types
    preserveEnv $ TypeResource $ ResourceTuple resources
  ExprLet pattern letExpr -> do
    (resource, env) <- inferPattern pattern
    exprType <- fst <$> inferExpr letExpr
    constrain $ TypeResource resource :~ exprType
    return (TypeResource ResourceUnit, env)
  ExprApply name arg -> do
    nameType <- lookupEnv name
    argType <- inferExpr arg >>= resourceType . fst
    valueType <- ResourceVariable <$> fresh
    constrain $ nameType :~ TypeInference (argType `Infers` valueType)
    preserveEnv $ TypeResource valueType
  ExprBlock exprs -> do
    env <- ask
    env' <- foldM foldBlock env $ NonEmpty.init exprs
    local (const env') $ inferExpr $ NonEmpty.last exprs
  where
    foldBlock env blockExpr = do
      (t, env') <- local (const env) $ inferExpr blockExpr
      constrain $ TypeResource ResourceUnit :~ t
      return env'

inferPattern :: Pattern -> Infer (Resource, Env)
inferPattern pattern = case pattern of
  PatternUnit -> preserveEnv ResourceUnit
  PatternBind name -> do
    env <- ask
    rv <- ResourceVariable <$> fresh
    return (rv, Map.insert name (ForAll [] $ TypeResource rv) env)
  PatternTuple patterns -> do
    results <- mapM inferPattern patterns
    let resources = fst <$> results
    let env = foldl1 Map.union $ snd <$> results
    return (ResourceTuple resources, env)

preserveEnv :: a -> Infer (a, Env)
preserveEnv x = do
  env <- ask
  return (x, env)

lookupEnv :: Name -> Infer Type
lookupEnv name = do
  env <- ask
  case Map.lookup name env of
    Just scheme -> instantiate scheme
    Nothing -> throwError $ UnboundVariable name

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

throwError :: TypeError -> Infer a
throwError = lift . lift . lift . throwE

getState :: Infer InferState
getState = lift $ lift get

putState :: InferState -> Infer ()
putState = lift . lift . put

constrain :: Constraint -> Infer ()
constrain = lift . tell . return
