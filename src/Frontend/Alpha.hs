module Frontend.Alpha (alpha) where

import Control.Arrow (first)
import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

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

suffixVar :: Name -> Alpha Name
suffixVar var = lift $ (var ++) . ('_':) . show <$> fresh

suffixBinder :: TypeLike t => Binder (Name, t) -> Alpha ([(Name, Name)], Binder (Name, t)) 
suffixBinder (Just (var, t)) = do
  var' <- suffixVar var
  return $ ([(var, var')], Just (var', t))
suffixBinder Nothing = return ([], Nothing) 

alphaExpr :: AnnExpr -> Alpha AnnExpr
alphaExpr (AVar var ts t) = do
  var' <- asks $ fromJust . M.lookup var
  return $ AVar var' ts t
alphaExpr (AValue val) = return $ AValue val
alphaExpr (AIf e1 e2 e3) =
  AIf <$> alphaExpr e1 <*> alphaExpr e2 <*> alphaExpr e3
alphaExpr (ALet d e) = do
  (s, d') <- alphaDecl d
  ALet d' <$> withSubst s (alphaExpr e)
alphaExpr (AMatch e alts) =
  AMatch <$> alphaExpr e <*> mapM alphaAlt alts
alphaExpr (AFun b e) = do
  (s, b') <- suffixBinder b
  AFun b' <$> withSubst s (alphaExpr e) 
alphaExpr (AApply e1 e2) =
  AApply <$> alphaExpr e1 <*> alphaExpr e2
alphaExpr (AOp op es) =
  AOp op <$> mapM alphaExpr es
alphaExpr (ACon con ts t es) =
  ACon con ts t <$> mapM alphaExpr es
alphaExpr (ATuple es) =
  ATuple <$> mapM alphaExpr es

alphaDecl :: AnnDecl -> Alpha ([(Name, Name)], AnnDecl)
alphaDecl (ARecDecl b e) = do
  (s, b') <- suffixBinder b
  (,) s . ARecDecl b' <$> withSubst s (alphaExpr e)
alphaDecl (ADecl b e) = do
  (s, b') <- suffixBinder b
  (,) s . ADecl b' <$> alphaExpr e

alphaAlt :: AnnAlt -> Alpha AnnAlt
alphaAlt (AConCase con ts bs e) = do
  (s, bs') <- first concat . unzip <$> mapM suffixBinder bs 
  AConCase con ts bs' <$> withSubst s  (alphaExpr e)
alphaAlt (ADefaultCase e) =
  ADefaultCase <$> alphaExpr e
