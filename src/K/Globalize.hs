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

import K.AST
import K.Types
import K.Alpha

import Internal

-- Globalize
-- TODO: Globalize values that can be allocated statically

globalize :: KProgram -> Fresh KProgram
globalize = bimapM return $ \decls -> do
  let env = M.fromList $ map bindOf decls
  let toplevels = S.fromList $ map (fst . bindOf) decls
  uncurry (flip (++) . catMaybes) <$> runGlob env toplevels (mapM globDecl decls)

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

typeOf :: Name -> Glob Type
typeOf name = asks $ fromJust . M.lookup name

globExpr :: KExpr -> Glob KExpr
globExpr (KIf cmp n1 n2 e1 e2) =
  KIf cmp n1 n2 <$> globExpr e1 <*> globExpr e2
globExpr (KLet d e) = do
  ds <- maybeToList <$> globDecl d
  flip (foldr KLet) ds <$> withBind (map bindOf ds) (globExpr e)
globExpr (KMatch n alts) =
  KMatch n <$> mapM globAlt alts
globExpr e = return e

globDecl :: KDecl -> Glob (Maybe KDecl)
globDecl (KFunDecl b@(n, t) bs e) = do
  e' <- withBind (b:bs) $ globExpr e
  toplevels <- get
  let vs = S.union toplevels $ S.fromList (n : map fst bs)
      fvs =  S.toList $ S.difference (freeVars e') vs
  case fvs of
    [] -> do
      addFunDecl $ KFunDecl b bs e'
      return Nothing
    _ -> do
      n' <- flip rename n <$> fresh
      n'' <- flip rename n <$> fresh
      ts <- mapM typeOf fvs
      fvs' <- mapM (\v -> flip rename v <$> fresh) fvs
      e'' <- lift . lift . lift $ alpha (M.fromList $ (n, n'') : zip fvs fvs') e' 
      let b' = (n', foldr FunType t ts)
          bs' = zip fvs' ts ++ bs
      addFunDecl $ KFunDecl b' bs' $ KLet (KDecl b (KApply n' fvs')) e''
      return $ Just $ KDecl b (KApply n' fvs)
globDecl (KDecl b e) =
  Just . KDecl b <$> globExpr e

globAlt :: KAlt -> Glob KAlt
globAlt (KConCase con bs e) =
  KConCase con bs <$> withBind bs (globExpr e)
globAlt (KDefaultCase e) =
  KDefaultCase <$> globExpr e
