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

-- Monomorphise

monomorphise :: AnnProgram -> Fresh AnnProgram
monomorphise = bimapM return $ runMono . monoExpr

type Instantiate = Map [Type] Name
type VariableUse = Map Name Instantiate
type Mono a = ReaderT TypeSubst (StateT VariableUse Fresh) a

runMono :: Mono a -> Fresh a
runMono = flip evalStateT M.empty . flip runReaderT M.empty

withSubst :: [(TypeVar, Type)] -> Mono a -> Mono a
withSubst s = local $ flip (foldr $ uncurry bind) s

replaceType :: Type -> Mono Type
replaceType t = f <$> asks (flip subst t)
 where
  -- replace free type variables with unit type
  f (DataType targs tcon) = DataType (map f targs) tcon
  f (TypeVar v) = UnitType
  f (FunType t1 t2) = FunType (f t1) (f t2)
  f (TupleType ts) = TupleType (map f ts)
  f t = t

useOf :: Name -> Mono Instantiate
useOf name = gets $ fromMaybe M.empty . M.lookup name

monoExpr :: AnnExpr -> Mono AnnExpr
monoExpr (AVar name []) = return $ AVar name []
monoExpr (AVar name targs) = do
  targs' <- mapM replaceType targs
  use <- useOf name
  case M.lookup targs' use of
    Just name' -> return $ AVar name' []
    Nothing -> do
      name' <- flip rename name <$> fresh
      modify $ M.insert name $ M.insert targs' name' use
      return $ AVar name' []
monoExpr (AValue val) = return $ AValue val
monoExpr (AIf e1 e2 e3) =
  AIf <$> monoExpr e1 <*> monoExpr e2 <*> monoExpr e3
monoExpr (ALet d e) =
  foldr ALet <$> monoExpr e <*> monoDecl d
monoExpr (AMatch e alts) =
  AMatch <$> monoExpr e <*> mapM monoAlt alts
monoExpr (AFun b e) =
  AFun <$> bimapM return replaceType b <*> monoExpr e
monoExpr (AApply e1 e2) =
  AApply <$> monoExpr e1 <*> monoExpr e2
monoExpr (AOp op es) =
  AOp op <$> mapM monoExpr es
monoExpr (ACon con tcon targs es) = do
  targs' <- mapM replaceType targs
  ACon con tcon targs' <$> mapM monoExpr es
monoExpr (ATuple es) =
  ATuple <$> mapM monoExpr es
monoExpr (AExt s t ns) = return $ AExt s t ns

monoDecl :: AnnDecl -> Mono [AnnDecl]
monoDecl (ARecDecl (name, TypeScheme [] t) e) = do
  t' <- replaceType t
  (:[]) . ARecDecl (name, TypeScheme [] t') <$> monoExpr e
monoDecl (ARecDecl (name, TypeScheme vs t) e) = do
  use <- useOf name
  forM (M.toList use) $ \(targs, name') -> do
    withSubst (zip vs targs) $ do
      t' <- replaceType t
      let alphaEnv = M.singleton name name'
      e' <- lift . lift $ alpha alphaEnv e
      ARecDecl (name', TypeScheme [] t') <$> monoExpr e'
monoDecl (ADecl (name, TypeScheme [] t) e) = do
  t' <- replaceType t
  (:[]) . ADecl (name, TypeScheme [] t') <$> monoExpr e
monoDecl (ADecl (name, TypeScheme vs t) e) = do
  use <- useOf name
  forM (M.toList use) $ \(targs, name') ->
    withSubst (zip vs targs) $ do
      t' <- replaceType t
      e' <- lift . lift $ alpha M.empty e
      ADecl (name', TypeScheme [] t') <$> monoExpr e'
monoDecl (ATupleDecl bs@((_, TypeScheme [] _):_) e) = do
  let ts = map (\(_, TypeScheme _ t) -> t) bs
  bs' <- zip (map fst bs) <$> mapM (replaceType >=> return . TypeScheme []) ts
  (:[]) . ATupleDecl bs' <$> monoExpr e
monoDecl (ATupleDecl bs@((_, TypeScheme vs _):_) e) = do
  uses <- mapM (useOf . fst) bs
  let ts = map (\(_, TypeScheme _ t) -> t) bs
      use = map (\targs -> (targs, map (fromMaybe Erased . M.lookup targs) uses)) $
              S.toList $ S.unions $ map M.keysSet uses
  forM use $ \(targs, names) ->
    withSubst (zip vs targs) $ do
      bs' <- zip names <$> mapM (replaceType >=> return . TypeScheme []) ts 
      e' <- lift . lift $ alpha M.empty e
      ATupleDecl bs' <$> monoExpr e'

monoAlt :: AnnAlt -> Mono AnnAlt
monoAlt (AConCase con tcon targs bs e) = do
  targs' <- mapM replaceType targs
  bs' <- mapM (bimapM return replaceType) bs
  AConCase con tcon targs' bs' <$> monoExpr e
monoAlt (ADefaultCase e) =
  ADefaultCase <$> monoExpr e
