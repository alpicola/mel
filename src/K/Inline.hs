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
inline = bimapM return $ runInline M.empty . inlineExpr

type InlineEnv = Map Name (KType, [Name], KExpr) 
type Inline a = ReaderT InlineEnv Fresh a

runInline :: InlineEnv -> Inline a -> Fresh a
runInline env = flip runReaderT env

withFunDecl :: KBinder -> [KBinder] -> KExpr -> Inline a -> Inline a
withFunDecl (n, t) bs e = local $ M.insert n (t, map fst bs, e)

inlineExpr :: KExpr -> Inline KExpr
inlineExpr (KIf cmp n1 n2 e1 e2) =
  KIf cmp n1 n2 <$> inlineExpr e1 <*> inlineExpr e2
inlineExpr (KLet (KFunDecl b@(n, _) bs e1) e2) =
  if sizeOf e1 > inliningThreashold || S.member n (freeVars e1)
    then
      KLet . KFunDecl b bs <$> inlineExpr e1 <*> inlineExpr e2
    else do
      e1' <- inlineExpr e1
      e2' <- withFunDecl b bs e1' $ inlineExpr e2
      return $ KLet (KFunDecl b bs e1') e2'
inlineExpr (KLet (KDecl b e1) e2) =
  KLet . KDecl b <$> inlineExpr e1 <*> inlineExpr e2
inlineExpr (KLet d e) =
  KLet d <$> inlineExpr e
inlineExpr (KMatch n alts) =
  KMatch n <$> mapM inlineAlt alts
inlineExpr (KApply n ns) = do
  m <- asks $ M.lookup n
  case m of
    Just (t, args, e)
      | i < j -> do
        n' <- Renamed "f" <$> fresh
        e' <- lift $ alpha (zip args ns) e
        return $ KLet (KDecl (n', returnType t i) e') $ KApply n' (drop i ns)
      | i == j -> lift $ alpha (zip args ns) e
      | i > j -> return $ KApply n ns
     where
      i = length args
      j = length ns
    Nothing -> return $ KApply n ns
inlineExpr e = return e

inlineAlt :: KAlt -> Inline KAlt
inlineAlt (KConCase con bs e) =
  KConCase con bs <$> inlineExpr e
inlineAlt (KDefaultCase e) =
  KDefaultCase <$> inlineExpr e

inliningThreashold :: Int
inliningThreashold = 30
