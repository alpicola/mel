module Frontend.Alpha (alpha) where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor

import Frontend.AST
import Frontend.Types
import Frontend.Dianostic
import Internal

alpha :: AnnProgram -> Fresh AnnProgram
alpha (typs, expr) = (,) typs <$> runAlpha (alphaExpr expr)

-- Alpha Transform

type AlphaEnv = Map Name Name
type Alpha a = ReaderT AlphaEnv Fresh a

runAlpha :: Alpha a -> Fresh a
runAlpha = flip runReaderT M.empty

withSubst :: [(Name, Name)] -> Alpha a -> Alpha a
withSubst s = local $ flip (foldr $ uncurry M.insert) s

alphaBinder :: TypeLike t => (Name, t) -> Alpha (Maybe (Name, Name), (Name, t))
alphaBinder b@(Erased, _) = return (Nothing, b)
alphaBinder (name, t) = do
  name' <- flip rename name <$> fresh
  return $ (Just (name, name'), (name', t))

alphaExpr :: AnnExpr -> Alpha AnnExpr
alphaExpr (AVar name@(External _) ts t) =
  return $ AVar name ts t
alphaExpr (AVar name ts t) = do
  name' <- asks $ fromJust . M.lookup name
  return $ AVar name' ts t
alphaExpr (AValue val) = return $ AValue val
alphaExpr (AIf e1 e2 e3) =
  AIf <$> alphaExpr e1 <*> alphaExpr e2 <*> alphaExpr e3
alphaExpr (ALet d e) = do
  (s, d') <- alphaDecl d
  ALet d' <$> withSubst s (alphaExpr e)
alphaExpr (AMatch e alts) =
  AMatch <$> alphaExpr e <*> mapM alphaAlt alts
alphaExpr (AFun b e) = do
  (s, b') <- first maybeToList <$> alphaBinder b
  AFun b' <$> withSubst s (alphaExpr e) 
alphaExpr (AApply e1 e2) =
  AApply <$> alphaExpr e1 <*> alphaExpr e2
alphaExpr (AOp op es) =
  AOp op <$> mapM alphaExpr es
alphaExpr (ACon con t es) =
  ACon con t <$> mapM alphaExpr es
alphaExpr (ATuple es) =
  ATuple <$> mapM alphaExpr es

alphaDecl :: AnnDecl -> Alpha ([(Name, Name)], AnnDecl)
alphaDecl (ARecDecl b e) = do
  (s, b') <- first maybeToList <$> alphaBinder b
  (,) s . ARecDecl b' <$> withSubst s (alphaExpr e)
alphaDecl (ADecl b e) = do
  (s, b') <- first maybeToList <$> alphaBinder b
  (,) s . ADecl b' <$> alphaExpr e
alphaDecl (ATupleDecl bs e) = do
  (s, bs') <- first catMaybes . unzip <$> mapM alphaBinder bs
  (,) s . ATupleDecl bs' <$> alphaExpr e

alphaAlt :: AnnAlt -> Alpha AnnAlt
alphaAlt (AConCase con t bs e) = do
  (s, bs') <- first catMaybes . unzip <$> mapM alphaBinder bs 
  AConCase con t bs' <$> withSubst s (alphaExpr e)
alphaAlt (ADefaultCase e) =
  ADefaultCase <$> alphaExpr e
