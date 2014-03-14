module K.Defunctionalize (defunctionalize) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Char (toUpper)
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Tuple
import Data.Bifunctor
import Data.Bitraversable

import Frontend.Builtins
import K.AST
import K.Types

import Internal

-- Defunctionalize

defunctionalize :: KProgram -> Fresh KProgram
defunctionalize (cons, decls) = do
  (table, closEnv1) <- buildClosTypes decls
  (decls', closEnv2) <- buildApply table
  let typeEnv = M.union (M.map toKType builtinFunctions) (M.fromList $ map bindOf decls)
  let env = (typeEnv, (closEnv1, closEnv2))
  let cons' = concatMap (flip zip [0..] . snd . snd) (M.toList closEnv2)
  (,) (cons ++ cons') <$> (runDefun env $ mapM defunDecl (decls' ++ decls))

-- Build closures datatypes and dispatch function for each fun type

type ClosType = (Name, [(Name, Name, [Type])])
type ClosConEnv = Map Name [Name]
type ClosTypeEnv = Map Type (Name, [Name])

buildClosTypes :: [KDecl] -> Fresh (Map Type ClosType, ClosConEnv)
buildClosTypes decls =
  swap . first (M.fromList . catMaybes) <$> runStateT (mapM f decls) M.empty 
 where
  f (KFunDecl (n@(Renamed s _), t) bs _) = do
    let tss = reverse $ drop 1 $ reverse $ inits (map snd bs)
        s' = capitalize s
    ns <- forM tss $ \ts -> do
      let t' = returnType t (length ts)
      n' <- Renamed s' <$> fresh
      let con = (n, n', ts)
      entry <- gets $ M.lookup t'
      case entry of
        Just (tcon, cons) -> modify $ M.insert t' (tcon, (con:cons))
        Nothing -> do
          tcon <- Renamed "clos" <$> fresh
          modify $ M.insert t' (tcon, [con])
      return n'
    return $ Just (n, ns)
  f (KDecl _ _) = return Nothing

  capitalize (c:cs) = toUpper c : cs
  capitalize [] = []

buildApply :: (Map Type ClosType) -> Fresh ([KDecl], ClosTypeEnv)
buildApply table =
  bimap concat M.fromList . unzip <$> mapM f (M.toList table)
 where
  f (t, (tcon, cons)) = do
    let tss = drop 1 $ inits $ fst $ uncurryType t (-1)
        t' = FunType (DataType [] tcon) t
    decls <- forM (zip tss [1..]) $ \(ts, i) -> do
      n <- Renamed "c" <$> fresh
      b <- flip (,) t' . Renamed ("apply" ++ show i) <$> fresh 
      bs <- mapM (\t -> flip (,) t . Renamed (prefixOfType t) <$> fresh) ts
      alts <- forM cons $ \(f, con, ts') -> do
        bs' <- mapM (\t -> flip (,) t . Renamed (prefixOfType t) <$> fresh) ts'
        return $ KConCase con bs' $ KApply f $ map fst (bs' ++ bs)
      return $ KFunDecl b ((n, DataType [] tcon) : bs) $ KMatch n alts  
    return (decls, (t, (tcon, map (fst . bindOf) decls)))

-- Replace function values with closure datatypes

type DefunEnv = (KTypeEnv, (ClosConEnv, ClosTypeEnv))
type Defun a = ReaderT DefunEnv Fresh a

runDefun :: DefunEnv -> Defun a -> Fresh a
runDefun env = flip runReaderT env

withBind :: [KBinder] -> Defun a -> Defun a
withBind bs = local $ first $ flip (foldr $ uncurry M.insert) bs'
 where
  bs' = filter (not . isErased . fst) bs

typeOf :: Name -> Defun Type
typeOf name = asks $ fromJust . M.lookup name . fst

replaceType :: Type -> Defun Type
replaceType t = f t
 where
  f t@(FunType _ _) =
    asks $ DataType [] . fst . fromJust . M.lookup t . snd . snd
  f (TupleType ts) = TupleType <$> mapM f ts
  f (DataType ts tcon) = flip DataType tcon <$> mapM f ts
  f t = return t

getApply :: Type -> Int -> Defun Name
getApply t i = asks $ (!! (i - 1)) . snd . fromJust . M.lookup t . snd . snd

defunVar :: Name -> Defun (Maybe KDecl, Name)
defunVar n = do 
  entry <- asks $ M.lookup n . fst . snd
  case entry of
    Just (con:_) -> do
      t <- typeOf n >>= replaceType
      n' <- Renamed "f" <$> fresh
      let d = KDecl (n', t) $ KCon con []
      return (Just d, n')
    Nothing -> return (Nothing, n)

defunBinder :: KBinder -> Defun KBinder 
defunBinder = bimapM return replaceType

defunExpr :: KExpr -> Defun KExpr
defunExpr (KVar n) = do
  (ds, n') <- first maybeToList <$> defunVar n
  return $ foldr KLet (KVar n') ds
defunExpr (KIf cmp n1 n2 e1 e2) =
  KIf cmp n1 n2 <$> defunExpr e1 <*> defunExpr e2
defunExpr (KLet d e) = do
  let b = bindOf d
  KLet <$> defunDecl d <*> withBind [b] (defunExpr e)
defunExpr (KMatch n alts) =
  KMatch n <$> mapM defunAlt alts
defunExpr (KApply n ns) = do
  (ds, ns') <- first catMaybes . unzip <$> mapM defunVar ns
  entry <- asks $ M.lookup n . fst . snd
  case entry of
    Just cons
      | i == j -> return $ foldr KLet (KApply n ns') ds
      | i > j -> return $ foldr KLet (KCon (cons !! j) ns') ds
      | otherwise -> do
        t <- typeOf n
        let t' = returnType t i
        n' <- Renamed "f" <$> fresh
        b <- (,) n' <$> replaceType t'
        f <- getApply t' (j - i)
        let d = KDecl b (KApply n (take i ns'))
        return $ foldr KLet (KApply f (n' : drop i ns')) (ds ++ [d])
     where
      i = length cons
      j = length ns'
    Nothing -> do
      t <- typeOf n
      f <- getApply t (length ns')
      return $ foldr KLet (KApply f (n:ns')) ds
defunExpr (KTuple ns) = do
  (ds, ns') <- first catMaybes . unzip <$> mapM defunVar ns
  return $ foldr KLet (KTuple ns') ds
defunExpr e = return e

defunDecl :: KDecl -> Defun KDecl 
defunDecl (KFunDecl b@(n, t) bs e) = do
  let (ts, t') = uncurryType t (length bs)
  b' <- (,) n . flip (foldr FunType) ts <$> replaceType t'
  KFunDecl b' <$> mapM defunBinder bs <*> withBind (b:bs) (defunExpr e)
defunDecl (KDecl b e) = do 
  KDecl <$> defunBinder b <*> defunExpr e 

defunAlt :: KAlt -> Defun KAlt
defunAlt (KConCase con bs e) =
  KConCase con <$> mapM defunBinder bs <*> defunExpr e
defunAlt (KDefaultCase e) =
  KDefaultCase <$> defunExpr e
