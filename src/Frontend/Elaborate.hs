{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Frontend.Elaborate (elaborate) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor

import Frontend.AST
import Frontend.Types
import Frontend.Values
import Frontend.Primitives
import Frontend.Builtins
import Frontend.Dianostic
import Internal

elaborate :: MLProgram -> Either Dianostic AnnProgram
elaborate (tdecls, decls) = do
  let conEnv = buildTypeConEnv builtinDataTypes
  typs <- (builtinDataTypes ++) <$> buildDataTypes conEnv tdecls
  let body = foldr MLLet (MLValue UnitValue) decls
      env = (M.map (TypeScheme []) builtinFunctions, buildConEnv typs)
  ((_, body'), s) <- runTC env $ tcExpr body
  return (typs, substExpr s body')

-- Process datatype definitions

type TypeConEnv = Map Name Int 
type ConEnv = Map Name (Int, Name, [Type])

buildDataTypes :: TypeConEnv -> [MLTypeDecl] -> Either Dianostic [DataTypeDecl]
buildDataTypes env decls = evalStateT (mapM build decls) env
 where
  build (params, tcon, cons) = do
    let len = length params
    env <- gets $ M.insert tcon len
    put env
    cons' <- lift $ forM cons $ \(name, args) ->
      (,) name <$> mapM (evalType params env) args
    return (len, tcon, cons')

evalType :: [Name] -> TypeConEnv -> MLType -> Either Dianostic Type
evalType params env t = eval t
 where
  eval (MLTypeVar tvar) =
    case elemIndex tvar params of
      Nothing -> unboundTypeVar tvar
      Just i -> return $ TypeVar i
  eval (MLTypeCon ts n@(Raw "int"))
    | null ts   = return IntType
    | otherwise = typeConArgsNum n (length ts) 0
  eval (MLTypeCon ts n@(Raw "float"))
    | null ts   = return FloatType
    | otherwise = typeConArgsNum n (length ts) 0
  eval (MLTypeCon ts n@(Raw "bool"))
    | null ts   = return BoolType
    | otherwise = typeConArgsNum n (length ts) 0
  eval (MLTypeCon ts n@(Raw "unit"))
    | null ts   = return UnitType
    | otherwise = typeConArgsNum n (length ts) 0
  eval (MLTypeCon ts tcon) =
    case M.lookup tcon env of
      Nothing -> unboundTypeCon tcon
      Just len
        | len == length ts -> flip DataType tcon <$> mapM eval ts
        | otherwise -> typeConArgsNum tcon (length ts) len 
  eval (MLFunType e e') = FunType <$> eval e <*> eval e'
  eval (MLTupleType es) = TupleType <$> mapM eval es

buildConEnv :: [DataTypeDecl] -> ConEnv
buildConEnv typs = M.fromList $ do
  (i, tcon, cons) <- typs
  (con, ts) <- cons
  return (con, (i, tcon, ts))

buildTypeConEnv :: [DataTypeDecl] -> TypeConEnv
buildTypeConEnv = M.fromList . map (\(i, tcon, _) -> (tcon, i))

-- Typecheck

type TCEnv = (PolyTypeEnv, ConEnv)

newtype TC a = TC
  { unTC :: ReaderT TCEnv (StateT TypeSubst (ErrorT Dianostic Fresh)) a }
 deriving ( Functor, Applicative, Monad, MonadError Dianostic
          , MonadReader TCEnv, MonadState TypeSubst, MonadFresh )

runTC :: TCEnv -> TC a -> Either Dianostic (a, TypeSubst)
runTC env = runFresh . runErrorT . flip runStateT M.empty . flip runReaderT env . unTC

genVar :: TC Type
genVar = TypeVar <$> fresh

withBind :: [PolyBinder] -> TC a -> TC a
withBind bs = local $ first $ flip (foldr $ uncurry M.insert) bs'
 where
  bs' = filter (not . isErased . fst) bs

update :: TypeLike t => t -> TC t
update t = gets $ flip subst t

generalize :: Type -> TC TypeScheme
generalize t = do
  t' <- update t
  env <- asks fst >>= update
  return $ TypeScheme (S.elems $ S.difference (fv t') (fv env)) t'

instantiate :: TypeScheme -> TC ([Type], Type)
instantiate (TypeScheme vs t) = do
  vs' <- mapM (const genVar) vs
  let s = M.fromList $ zip vs vs'
  (,) vs' . subst s <$> update t

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
tcExpr (MLVar name) = do
  env <- asks fst
  case M.lookup name env of
    Just sc -> do
      (targs, t) <- instantiate sc
      return (t, AVar name targs)
    Nothing -> do
      let name' = case name of Raw n -> External n
      case M.lookup name' env of
        Just (TypeScheme [] t) ->
          return (t, AVar name' [])
        Nothing -> unboundVar name
tcExpr (MLValue val) =
  return (typeOf val, AValue val)
tcExpr (MLIf e1 e2 e3) = do
  (t1, e1') <- tcExpr e1
  unify t1 BoolType
  (t2, e2') <- tcExpr e2
  (t3, e3') <- tcExpr e3
  unify t2 t3
  return (t3, AIf e1' e2' e3')
tcExpr (MLLet d e) = do
  (bs, d') <- tcDecl d
  (t, e') <- withBind bs $ tcExpr e
  return (t, ALet d' e')
tcExpr (MLMatch e alts) = do
  (t, e') <- tcExpr e
  v <- genVar
  alts' <- forM alts $ \alt -> do
    (t1, t2, alt') <- tcAlt alt
    unify t t1
    unify v t2
    return alt'
  return (v, AMatch e' alts')
tcExpr (MLFun b e) = do
  v <- genVar
  (t, e') <- withBind [(b, TypeScheme [] v)] $ tcExpr e
  return (FunType v t, AFun (b, t) e')
tcExpr (MLApply e1 e2) = do
  (t1, e1') <- tcExpr e1
  (t2, e2') <- tcExpr e2
  v <- genVar
  unify t1 (FunType t2 v)
  return (v, AApply e1' e2')
tcExpr (MLOp op@(Cmp _) es) = do
  v <- genVar
  es' <- forM es $ \e -> do
    (t, e') <- tcExpr e
    unify v t
    return e'
  return (BoolType, AOp op es')
tcExpr (MLOp op@(Arith _) es) = do
  es' <- forM es $ \e -> do
    (t, e') <- tcExpr e
    unify IntType t
    return e'
  return (IntType, AOp op es')
tcExpr (MLOp op@(FArith _) es) = do
  es' <- forM es $ \e -> do
    (t, e') <- tcExpr e
    unify FloatType t
    return e'
  return (FloatType, AOp op es')
tcExpr (MLCon con es) = do
  env <- asks snd
  case M.lookup con env of
    Nothing -> unboundCon con
    Just (i, tcon, ts)
      | length ts == length es -> do
        let sc = TypeScheme [0..i-1] $ TupleType ts
        (targs, TupleType ts') <- instantiate sc
        (ts'', es') <- unzip <$> mapM tcExpr es
        zipWithM_ unify ts' ts''
        let t = DataType targs tcon
        return (t, ACon con tcon targs es')
      | otherwise -> conArgsNum con (length ts) (length es)  
tcExpr (MLTuple es) = do
  (ts, es') <- unzip <$> mapM tcExpr es
  return (TupleType ts, ATuple es')

tcDecl :: MLDecl -> TC ([PolyBinder], AnnDecl)
tcDecl (MLRecDecl b e) = do
  v <- genVar
  (t, e') <- withBind [(b, TypeScheme [] v)] $ tcExpr e
  sc <- if isValue e' then generalize t
                      else return $ TypeScheme [] t
  let b' = (b, sc)
  return ([b'], ARecDecl b' e')
tcDecl (MLDecl b e) = do
  (t, e') <- tcExpr e
  sc <- if isValue e' then generalize t
                      else return $ TypeScheme [] t
  let b' = (b, sc)
  return ([b'], ADecl b' e')
tcDecl (MLUnitDecl e) = do
  (t, e') <- tcExpr e
  unify UnitType t
  let b = (Erased, TypeScheme [] t)
  return ([], ADecl b e')
tcDecl (MLTupleDecl bs e) = do
  vs <- mapM (const genVar) bs
  (t, e') <- tcExpr e
  unify t (TupleType vs)
  sc <- if isValue e' then generalize t
                      else return $ TypeScheme [] t
  let bs' = case sc of
              TypeScheme vs' (TupleType ts) ->
                zip bs $ map (TypeScheme vs') ts
  return (bs', ATupleDecl bs' e')

tcAlt :: MLAlt -> TC (Type, Type, AnnAlt)
tcAlt (MLConCase con bs e) = do
  env <- asks snd
  case M.lookup con env of
    Nothing -> unboundCon con
    Just (i, tcon, ts) -> do
      let sc = TypeScheme [0..i-1] $ TupleType ts
      (targs, TupleType ts') <- instantiate sc
      let t = DataType targs tcon
          bs' = zip bs ts'
      (t', e') <- withBind (map (second $ TypeScheme []) bs') $ tcExpr e
      return (t, t', AConCase con tcon targs bs' e')
tcAlt (MLDefaultCase e) = do
  v <- genVar
  (t, e') <- tcExpr e
  return (v, t, ADefaultCase e')
