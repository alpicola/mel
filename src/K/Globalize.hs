module K.Globalize (globalize) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor
import Data.Bitraversable

import Frontend.Builtins
import Frontend.Primitives
import K.AST
import K.Types

import Internal

-- Globalize

globalize :: KProgram -> Fresh KProgram
globalize = bimapM return $ \decls -> do
  let env = M.union (M.map toKType builtinFunctions)
                    (M.fromList $ map bindOf decls)
  let toplevels = S.fromList $ map (fst . bindOf) decls
  uncurry (flip (++)) <$> runGlob env toplevels (mapM go decls)
 where
  go (KFunDecl b bs e) =
    KFunDecl b bs <$> withBind (b:bs) (globExpr e)
  go (KDecl b e) =
    KDecl b <$> withBind [b] (globExpr e)

type Glob a = ReaderT KTypeEnv (StateT (Set Name) (WriterT [KDecl] Fresh)) a

runGlob :: KTypeEnv -> Set Name -> Glob a -> Fresh (a, [KDecl])
runGlob env toplevels = runWriterT . flip evalStateT toplevels . flip runReaderT env

withBind :: [KBinder] -> Glob a -> Glob a
withBind bs = local $ flip (foldr $ uncurry M.insert) bs'
 where
  bs' = filter (not . isErased . fst) bs

addFunDecl :: KDecl -> Glob ()
addFunDecl decl = do
  tell [decl]
  modify $ S.insert (fst $ bindOf $ decl)

askTypeOf :: Name -> Glob Type
askTypeOf name = asks $ fromJust . M.lookup name

globExpr :: KExpr -> Glob KExpr
globExpr (KIf cmp n1 n2 e1 e2) =
  KIf cmp n1 n2 <$> globExpr e1 <*> globExpr e2
globExpr (KLet (KFunDecl b@(n, t) bs e1) e2) = do
  e1' <- withBind (b:bs) $ globExpr e1
  toplevels <- get
  let vs = S.union toplevels $ S.fromList (n : map fst bs)
      fvs =  S.toList $ S.difference (freeVars e1') vs
  case fvs of
    [] -> do
      addFunDecl $ KFunDecl b bs e1'
      globExpr e2
    _ -> do
      ts <- mapM askTypeOf fvs
      n' <- flip rename n <$> fresh
      fvs' <- mapM (\v -> flip rename v <$> fresh) fvs
      let b' = (n', foldr FunType t ts)
      addFunDecl $ KFunDecl b' (zip fvs' ts ++ bs) $
        KLet (KDecl b (KApply n' fvs')) e1' 
      KLet (KDecl b (KApply n' fvs)) <$> withBind [b] (globExpr e2)
globExpr (KLet (KDecl b e1) e2) = do
  KLet . KDecl b <$> globExpr e1 <*> withBind [b] (globExpr e2)
globExpr (KMatch n alts) =
  KMatch n <$> mapM globAlt alts
globExpr e = return e

globAlt :: KAlt -> Glob KAlt
globAlt (KConCase con bs e) =
  KConCase con bs <$> withBind bs (globExpr e)
globAlt (KDefaultCase e) =
  KDefaultCase <$> globExpr e
