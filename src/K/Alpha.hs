module K.Alpha (alpha) where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor

import K.AST

import Internal

alpha :: [(Name, Name)] -> KExpr -> Fresh KExpr
alpha env expr = runAlpha (M.fromList env) $ alphaExpr expr

-- Alpha Transform

type AlphaEnv = Map Name Name
type Alpha a = ReaderT AlphaEnv Fresh a

runAlpha :: AlphaEnv -> Alpha a -> Fresh a
runAlpha = flip runReaderT

withSubst :: [(Name, Name)] -> Alpha a -> Alpha a
withSubst s = local $ flip (foldr $ uncurry M.insert) s

pullName :: Name -> Alpha Name
pullName name = asks $ fromMaybe name . M.lookup name

alphaBinder :: KBinder -> Alpha (KBinder, Maybe (Name, Name))
alphaBinder (name, t) = do
  name' <- flip rename name <$> fresh
  if name' == name
    then return $ ((name', t), Nothing)
    else return $ ((name', t), Just (name, name'))

alphaExpr :: KExpr -> Alpha KExpr
alphaExpr (KVar name) = do
  KVar <$> asks (fromMaybe name . M.lookup name)
alphaExpr (KValue val) = return $ KValue val
alphaExpr (KIf cmp n1 n2 e1 e2) =
  KIf cmp <$> pullName n1 <*> pullName n2 <*> alphaExpr e1 <*> alphaExpr e2
alphaExpr (KLet d e) = do
  (d', s) <- alphaDecl d
  KLet d' <$> withSubst s (alphaExpr e)
alphaExpr (KMatch n alts) =
  KMatch <$> pullName n <*> mapM alphaAlt alts
alphaExpr (KApply n ns) =
  KApply <$> pullName n <*> mapM pullName ns
alphaExpr (KOp op ns) =
  KOp op <$> mapM pullName ns
alphaExpr (KCon con ns) =
  KCon con <$> mapM pullName ns
alphaExpr (KTuple ns) =
  KTuple <$> mapM pullName ns

alphaDecl :: KDecl -> Alpha (KDecl, [(Name, Name)])
alphaDecl (KFunDecl b bs e) = do
  (b', s1) <- second maybeToList <$> alphaBinder b
  (bs', s2) <- second catMaybes . unzip <$> mapM alphaBinder bs
  let s = s1 ++ s2
  flip (,) s . KFunDecl b' bs' <$> withSubst s (alphaExpr e)
alphaDecl (KDecl b e) = do
  (b', s) <- second maybeToList <$> alphaBinder b
  flip (,) s . KDecl b' <$> alphaExpr e
alphaDecl (KTupleDecl bs n) = do
  (bs', s) <- second catMaybes . unzip <$> mapM alphaBinder bs
  flip (,) s . KTupleDecl bs' <$> pullName n


alphaAlt :: KAlt -> Alpha KAlt
alphaAlt (KConCase con bs e) = do
  (bs', s) <- second catMaybes . unzip <$> mapM alphaBinder bs
  KConCase con bs' <$> withSubst s (alphaExpr e)
alphaAlt (KDefaultCase e) =
  KDefaultCase <$> alphaExpr e
