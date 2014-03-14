module K.Inline (inline) where

import Control.Applicative
import Control.Monad.Reader
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Bifunctor
import Data.Bitraversable

import K.AST
import K.Types
import K.Alpha

import Internal

-- Inlining

inline :: KProgram -> Fresh KProgram
inline = bimapM return $ \decls -> do
  let env = M.fromList $ map (\d -> (fst (bindOf d), d)) $ filter isInlinable decls
  runInline env $ mapM inlineDecl decls

type InlineEnv = Map Name KDecl
type Inline a = ReaderT InlineEnv Fresh a

runInline :: InlineEnv -> Inline a -> Fresh a
runInline env = flip runReaderT env

withFunDecl :: KDecl -> Inline a -> Inline a
withFunDecl d = local $ M.insert (fst (bindOf d)) d

getFunDecl :: Name -> Inline (Maybe KDecl)
getFunDecl n = asks $ M.lookup n

inlineExpr :: KExpr -> Inline KExpr
inlineExpr (KIf cmp n1 n2 e1 e2) =
  KIf cmp n1 n2 <$> inlineExpr e1 <*> inlineExpr e2
inlineExpr (KLet d@(KFunDecl _ _ _) e) = do
  d' <- inlineDecl d
  if isInlinable d'
    then KLet d' <$> withFunDecl d' (inlineExpr e)
    else KLet d' <$> inlineExpr e
inlineExpr (KLet b e) =
  KLet <$> inlineDecl b <*> inlineExpr e
inlineExpr (KMatch n alts) =
  KMatch n <$> mapM inlineAlt alts
inlineExpr (KApply n ns) = do
  entry <- getFunDecl n
  case entry of
    Just (KFunDecl (n, t) bs e)
      | i == j -> lift $ alpha (M.fromList $ zip (map fst bs) ns) e
      | i > j -> return $ KApply n ns
      | i < j -> do
        n' <- Renamed "f" <$> fresh
        e' <- lift $ alpha (M.fromList $ zip (map fst bs) ns) e
        let d = KDecl (n', snd (uncurryType t i)) e'
        return $ KLet d $ KApply n' (drop i ns)
     where
      i = length bs
      j = length ns
    Nothing -> return $ KApply n ns
inlineExpr e = return e

inlineDecl :: KDecl -> Inline KDecl
inlineDecl (KFunDecl b bs e) =
  KFunDecl b bs <$> inlineExpr e
inlineDecl (KDecl b e) =
  KDecl b <$> inlineExpr e

inlineAlt :: KAlt -> Inline KAlt
inlineAlt (KConCase con bs e) =
  KConCase con bs <$> inlineExpr e
inlineAlt (KDefaultCase e) =
  KDefaultCase <$> inlineExpr e

isInlinable :: KDecl -> Bool
isInlinable (KFunDecl (n, _) _ e) =
  sizeOf e <= inliningThreashold && S.notMember n (freeVars e)
isInlinable (KDecl _ _) = False

inliningThreashold :: Int
inliningThreashold = 30
