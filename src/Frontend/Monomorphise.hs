{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module Frontend.Monomorphise (monomorphise) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor
import Data.Bitraversable

import Frontend.AST
import Frontend.Types
import Frontend.Alpha
import Internal

monomorphise :: AnnProgram -> Fresh AnnProgram
monomorphise (typs, expr) =
  runMono $ flip (,) <$> monoExpr expr <*> fmap concat (mapM monoDataType typs)

-- Monomorphise

type Instantiate = Map Name (Map [Type] Name)
type DataTypeUse = Instantiate
type VariableUse = Instantiate
type MonoState = (DataTypeUse, VariableUse)

newtype Mono a = Mono
  { unMono :: ReaderT TypeSubst (StateT MonoState Fresh) a }
 deriving ( Functor, Applicative, Monad, MonadReader TypeSubst
          , MonadState MonoState, MonadFresh )

runMono :: Mono a -> Fresh a
runMono = flip evalStateT (M.empty, M.empty) . flip runReaderT M.empty . unMono

update :: Type -> Mono Type
update t = asks (flip subst t) >>= f 
 where
  f (DataType [] tcon) = return $ DataType [] tcon
  f (DataType targs tcon) = do
    targs' <- mapM f targs
    use <- gets $ fromMaybe M.empty . M.lookup tcon . fst
    case M.lookup targs' use of
      Just tcon' -> return $ DataType [] tcon'
      Nothing -> do
        tcon' <- flip rename tcon <$> fresh
        modify $ first $ M.insert tcon $ M.insert targs' tcon' use
        return $ DataType [] tcon'
  f (TypeVar v) = return UnitType 
  f (FunType t1 t2) = FunType <$> f t1 <*> f t2
  f (TupleType ts) = TupleType <$> mapM f ts
  f t = return t

monoDataType :: DataTypeDecl -> Mono [DataTypeDecl]
monoDataType (0, tcon, cons) = return [(0, tcon, cons)]
monoDataType (i, tcon, cons) = do
  use <- gets $ fromMaybe M.empty . M.lookup tcon . fst
  forM (M.toList use) $ \(ts, tcon'@(Renamed _ k)) ->
    local (flip (foldr $ uncurry bind) $ zip [0..i-1] ts) $ do
      cons' <- mapM (bimapM (return . rename k) (mapM update)) cons
      return (i, tcon', cons')

monoExpr :: AnnExpr -> Mono AnnExpr
monoExpr (AVar name []) = return $ AVar name []
monoExpr (AVar name targs) = do
  targs' <- mapM update targs
  use <- gets $ fromMaybe M.empty . M.lookup name . snd
  case M.lookup targs' use of
    Just name' -> return $ AVar name' []
    Nothing -> do
      name' <- flip rename name <$> fresh
      modify $ second $ M.insert name $ M.insert targs' name' use
      return $ AVar name' []
monoExpr (AValue val) = return $ AValue val
monoExpr (AIf e1 e2 e3) =
  AIf <$> monoExpr e1 <*> monoExpr e2 <*> monoExpr e3
monoExpr (ALet d e) =
  foldr ALet <$> monoExpr e <*> monoDecl d
monoExpr (AMatch e alts) =
  AMatch <$> monoExpr e <*> mapM monoAlt alts
monoExpr (AFun b e) =
  AFun <$> bimapM return update b <*> monoExpr e
monoExpr (AApply e1 e2) =
  AApply <$> monoExpr e1 <*> monoExpr e2
monoExpr (AOp op es) =
  AOp op <$> mapM monoExpr es
monoExpr (ACon con t es) = do
  t' <- update t
  case t' of
    DataType _ (Renamed _ i) ->
      ACon (rename i con) t' <$> mapM monoExpr es
    _ -> ACon con t' <$> mapM monoExpr es
monoExpr (ATuple es) =
  ATuple <$> mapM monoExpr es

monoDecl :: AnnDecl -> Mono [AnnDecl]
monoDecl (ARecDecl (name, TypeScheme [] t) e) = do
  t' <- update t
  (:[]) . ARecDecl (name, TypeScheme [] t') <$> monoExpr e
monoDecl (ARecDecl (name, TypeScheme vs t) e) = do
  use <- gets $ fromMaybe M.empty . M.lookup name . snd
  forM (M.toList use) $ \(targs, name') -> do
    local (flip (foldr $ uncurry bind) $ zip vs targs) $ do
      t' <- update t
      let alphaEnv = M.singleton name name'
      e' <- Mono . lift . lift $ runAlpha alphaEnv $ alphaExpr e
      ARecDecl (name', TypeScheme [] t') <$> monoExpr e'
monoDecl (ADecl (name, TypeScheme [] t) e) = do
  t' <- update t
  (:[]) . ADecl (name, TypeScheme [] t') <$> monoExpr e
monoDecl (ADecl (name, TypeScheme vs t) e) = do
  use <- gets $ fromMaybe M.empty . M.lookup name . snd
  forM (M.toList use) $ \(targs, name') ->
    local (flip (foldr $ uncurry bind) $ zip vs targs) $ do
      t' <- update t
      e' <- Mono . lift . lift $ runAlpha M.empty $ alphaExpr e
      ADecl (name', TypeScheme [] t') <$> monoExpr e'
monoDecl (ATupleDecl bs@((_, TypeScheme [] _):_) e) = do
  let ts = map (\(_, TypeScheme _ t) -> t) bs
  bs' <- zip (map fst bs) <$> mapM (update >=> return . TypeScheme []) ts
  (:[]) . ATupleDecl bs' <$> monoExpr e
monoDecl (ATupleDecl bs@((_, TypeScheme vs _):_) e) = do
  uses <- mapM (\(name, _) -> gets $ fromMaybe M.empty . M.lookup name . snd) bs
  let ts = map (\(_, TypeScheme _ t) -> t) bs
      use = map (\targs -> (targs, map (fromMaybe Erased . M.lookup targs) uses)) $
              S.toList $ S.unions $ map M.keysSet uses
  forM use $ \(targs, names) ->
    local (flip (foldr $ uncurry bind) $ zip vs targs) $ do
      bs' <- zip names <$> mapM (update >=> return . TypeScheme []) ts 
      e' <- Mono . lift . lift $ runAlpha M.empty $ alphaExpr e
      ATupleDecl bs' <$> monoExpr e'

monoAlt :: AnnAlt -> Mono AnnAlt
monoAlt (AConCase con t bs e) = do
  t' <- update t
  bs' <- mapM (bimapM return update) bs
  case t' of
    DataType _ (Renamed _ i) ->
      AConCase (rename i con) t' bs' <$> monoExpr e 
    _ -> AConCase con t' bs' <$> monoExpr e
monoAlt (ADefaultCase e) =
  ADefaultCase <$> monoExpr e
