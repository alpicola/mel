{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Frontend.Elaborate (elaborate) where

import Control.Arrow (first, second)
import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S

import Frontend.AST
import Frontend.Types
import Frontend.Dianostic
import Internal

elaborate :: MLProgram -> Frontend AnnProgram
elaborate (tdecls, decls) = do
  typs <- (builtinDataTypes ++) <$> buildDataTypes tdecls
  let body = foldr MLLet (MLValue UnitValue) decls
      env = (M.empty, buildConEnv typs)
  ((_, body'), s) <- runTC env $ tcExpr body
  return (typs, substExpr s body')

-- Process datatype definitions

type TypeConEnv = Map Name Int 
type ConEnv = Map Name (Int, Type, [Type])

builtinTypeConEnv :: TypeConEnv
builtinTypeConEnv =
  M.fromList $ concat $ zipWith (map . flip (,)) [0..] builtins
 where
  builtins = [ ["int", "float", "bool", "unit"]
             , ["list", "option"]
             ]

builtinDataTypes :: [DataType]
builtinDataTypes =
  [ (1, "list", [("[]", []), ("::", [TypeVar 0, DataType [TypeVar 0] "list"])])
  , (1, "option", [("None", []), ("Some", [TypeVar 0])])
  ]

buildDataTypes :: [MLTypeDecl] -> Frontend [DataType]
buildDataTypes decls = evalStateT (mapM build decls) builtinTypeConEnv
 where
  build (params, tcon, cons) = do
    let len = length params
    env <- gets $ M.insert tcon len
    put env
    cons' <- lift $ forM cons $ \(name, args) ->
      (,) name <$> mapM (evalType params env) args
    return (len, tcon, cons')

evalType :: [Name] -> TypeConEnv -> MLType -> Frontend Type
evalType params env t = eval t
 where
  eval (MLTypeVar tvar) =
    case elemIndex tvar params of
      Nothing -> unboundTypeVar tvar
      Just i -> return $ TypeVar i
  eval (MLTypeCon targs tcon) =
    case M.lookup tcon env of
      Nothing -> unboundTypeCon tcon
      Just len
        | len == length targs ->
          case tcon of
            "int" -> return IntType
            "float" -> return FloatType
            "bool" -> return BoolType
            "unit" -> return UnitType
            _ -> flip DataType tcon <$> mapM eval targs 
        | otherwise -> typeConArgsNum tcon (length targs) len 
  eval (MLFunType e e') = FunType <$> eval e <*> eval e'
  eval (MLTupleType es) = TupleType <$> mapM eval es

buildConEnv :: [DataType] -> ConEnv
buildConEnv typs = M.fromList $ do
  (i, tcon, cons) <- typs
  (con, ts) <- cons
  return (con, (i, DataType (map TypeVar [0..i-1]) tcon, ts))

-- Typecheck

type TCEnv = (TypeEnv, ConEnv)

newtype TC a = TC
  { unTC :: ReaderT TCEnv (StateT TypeSubst (ErrorT Dianostic Fresh)) a }
 deriving ( Functor, Applicative, Monad, MonadError Dianostic
          , MonadReader TCEnv, MonadState TypeSubst, MonadFresh )

runTC :: TCEnv -> TC a -> Frontend (a, TypeSubst)
runTC env = flip runStateT M.empty . flip runReaderT env . unTC

genVar :: TC Type
genVar = TypeVar <$> fresh

withBind :: [(Name, TypeScheme)] -> TC a -> TC a
withBind bs = local $ first $ flip (foldr $ uncurry M.insert) bs

update :: TypeLike t => t -> TC t
update t = gets $ flip subst t

generalize :: Type -> TC TypeScheme
generalize t = do
  t' <- update t
  env <- asks fst >>= update
  return $ TypeScheme (S.elems $ S.difference (fv t') (fv env)) t'

instatiate :: TypeScheme -> TC Type
instatiate (TypeScheme vs t) = do
  s <- M.fromList <$> mapM (\v -> (v,) <$> genVar) vs
  subst s <$> update t

instatiate' :: [TypeVar] -> [Type] -> TC [Type]
instatiate' vs ts = do
  s <- M.fromList <$> mapM (\v -> (v,) <$> genVar) vs
  mapM (update >=> return . subst s) ts

bindVar :: TypeVar -> Type -> TC ()
bindVar v t
  | TypeVar v == t = return ()
  | S.member v (fv t) = imcompatibleTypes (TypeVar v) t
  | otherwise = modify $ bind v t

unify :: Type -> Type -> TC ()
unify t1 t2 = do
  t1' <- update t1
  t2' <- update t2
  unify' t1' t2'
 where
  unify' (TypeVar v) t = bindVar v t
  unify' t (TypeVar v) = bindVar v t
  unify' (FunType t1 t2) (FunType t1' t2') = unify t1 t1' >> unify t2 t2'
  unify' t@(TupleType ts) t'@(TupleType ts')
    | length ts == length ts' =  zipWithM_ unify ts ts'
    | otherwise = imcompatibleTypes t t'
  unify' t@(DataType ts n) t'@(DataType ts' n')
    | n == n' = zipWithM_ unify ts ts'
    | otherwise = imcompatibleTypes t t'
  unify' t t'
    | t == t' = return ()
    | otherwise = imcompatibleTypes t t'

tcExpr :: MLExpr -> TC (Type, AnnExpr)
tcExpr (MLVar n) = do
  env <- asks fst
  case M.lookup n env of
    Nothing -> unboundVar n
    Just t -> do t' <- instatiate t
                 return (t', AVar n t')
tcExpr (MLValue val) =
  let t = case val of
            IntValue _ -> IntType
            FloatValue _ -> FloatType
            BoolValue _ -> BoolType
            UnitValue -> UnitType
  in return (t, AValue val)
tcExpr (MLIf e1 e2 e3) = do
  (t1, e1') <- tcExpr e1
  unify t1 BoolType
  (t2, e2') <- tcExpr e2
  (t3, e3') <- tcExpr e3
  unify t2 t3
  return (t3, AIf e1' e2' e3')
tcExpr (MLLet d e) = do
  (bind, d') <- tcDecl d
  (t, e') <- withBind bind $ tcExpr e
  return (t, ALet d' e')
tcExpr (MLMatch e alts) = do
  (t, e') <- tcExpr e
  v <- genVar
  alts' <- forM alts $ \alt -> do
    (t1, t2, alt') <- tcAlt alt
    unify t t1
    unify v t2
    return alt'
  return (v, AMatch e' alts' t)
tcExpr (MLFun b e) = do
  v <- genVar
  (t, e') <- withBind (maybeToList $ (, mono v) <$> b) $ tcExpr e
  let b' = (, v) <$> b
  return (FunType v t, AFun b' e')
tcExpr (MLApply e1 e2) = do
  (t1, e1') <- tcExpr e1
  (t2, e2') <- tcExpr e2
  v <- genVar
  unify t1 (FunType t2 v)
  return (v, AApply e1' e2')
tcExpr (MLOp op es)
  | isCompOp op = do
    v <- genVar
    es' <- forM es $ \e -> do
      (t, e') <- tcExpr e
      unify v t
      return e'
    return (BoolType, AOp op es')
  | isArithOp op = do
    es' <- forM es $ \e -> do
      (t, e') <- tcExpr e
      unify IntType t
      return e'
    return (IntType, AOp op es')
  | isFArithOp op = do
    es' <- forM es $ \e -> do
      (t, e') <- tcExpr e
      unify FloatType t
      return e'
    return (FloatType, AOp op es')
tcExpr (MLCon con args) = do
  env <- asks snd
  case M.lookup con env of
    Nothing -> unboundCon con
    Just (i, t, targs)
      | length targs == length args -> do
        (t':targs') <- instatiate' [0..i-1] (t:targs)
        (ts, args') <- unzip <$> mapM tcExpr args
        zipWithM_ unify ts targs'
        return (t', ACon con args' t' targs')
      | otherwise -> conArgsNum con (length targs) (length args)  

tcDecl :: MLDecl -> TC ([(Name, TypeScheme)], AnnDecl)
tcDecl (MLRecDecl b e) = do
  v <- genVar
  (t, e') <- withBind (maybeToList $ (, mono v) <$>  b) $ tcExpr e
  t' <- generalize t
  let b' = (, t') <$> b
  return (maybeToList b', ARecDecl b' e')
tcDecl (MLDecl b e) = do
  (t, e') <- tcExpr e
  t' <- generalize t
  let b' = (, t') <$> b
  return (maybeToList b', ADecl b' e')

tcAlt :: MLAlt -> TC (Type, Type, AnnAlt)
tcAlt (MLConCase con bs e) = do
  env <- asks snd
  case M.lookup con env of
    Nothing -> unboundCon con
    Just (i, t, targs) -> do
      (t':targs') <- instatiate' [0..i-1] (t:targs)
      let bs' = zipWith (\b t -> (, t) <$> b) bs targs'
          bind = map (second mono) $ catMaybes bs'
      (t'', e') <- withBind bind $ tcExpr e
      return (t', t'', AConCase con bs' e')
tcAlt (MLDefaultCase e) = do
  v <- genVar
  (t, e') <- tcExpr e
  return (v, t, ADefaultCase e')

-- replace free type variables with an arbitrary type
-- subst' :: TypeLike t => TypeSubst -> t -> t
-- subst' s t = let s' = M.fromList $ map (flip (,) UnitType) $ S.elems $ fv t
--              in subst s' $ subst s t

substBinder :: TypeLike t => TypeSubst -> Maybe (a, t) -> Maybe (a, t)
substBinder s = fmap $ second $ subst s

substExpr :: TypeSubst -> AnnExpr -> AnnExpr
substExpr s (AVar n t) = AVar n $ subst s t
substExpr s e@(AValue _) = e
substExpr s (AIf e1 e2 e3) = AIf (substExpr s e1) (substExpr s e2) (substExpr s e3)
substExpr s (ALet d e) = ALet (substDecl s d) (substExpr s e) 
substExpr s (AMatch e alts t) = AMatch (substExpr s e) (map (substAlt s) alts) (subst s t)
substExpr s (AFun b e) = AFun (substBinder s b) (substExpr s e) 
substExpr s (AApply e1 e2) = AApply (substExpr s e1) (substExpr s e2)
substExpr s (ACon con es t ts) = ACon con (map (substExpr s) es) (subst s t) (map (subst s) ts)
substExpr s (AOp op es) = AOp op (map (substExpr s) es)

substDecl :: TypeSubst -> AnnDecl -> AnnDecl
substDecl s (ARecDecl b e) = ARecDecl (substBinder s b) (substExpr s e)
substDecl s (ADecl b e) = ADecl (substBinder s b) (substExpr s e)

substAlt :: TypeSubst -> AnnAlt -> AnnAlt
substAlt s (AConCase con bs e) = AConCase con (map (substBinder s) bs) (substExpr s e)
substAlt s (ADefaultCase e) = ADefaultCase (substExpr s e)
