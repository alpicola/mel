module K.Normalize (normalize) where

import Control.Applicative
import Control.Monad.Reader
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor
import Data.Bitraversable

import Frontend.AST
import Frontend.Types
import Frontend.Values
import Frontend.Builtins
import Frontend.Primitives
import K.AST
import K.Types

import Internal

-- K-normalization

normalize :: AnnProgram -> Fresh KProgram
normalize (typs, expr) = do
  let env = M.map toKType builtinFunctions
  (expr', t) <- runNorm env $ normExpr expr
  return (buildConDecls typs, [KDecl (Special "main", t) expr'])

-- To simplify datatypes declarations

buildConDecls :: [DataTypeDecl] -> [KConDecl]
buildConDecls typs = do
  (_, _, cons) <- typs
  zip (map fst cons) [0..]

-- Bind all intermediate values to variables

type Norm a = ReaderT KTypeEnv Fresh a

runNorm :: KTypeEnv -> Norm a -> Fresh a
runNorm env = flip runReaderT env

withBind :: [KBinder] -> Norm a -> Norm a
withBind bs = local $ flip (foldr $ uncurry M.insert) bs'
 where
  bs' = filter (not . isErased . fst) bs

bindExpr :: Norm (KExpr, KType) -> (KBinder -> Norm (KExpr, KType)) -> Norm (KExpr, KType)
bindExpr m k = do
  (e, t) <- m
  case e of
    KVar name -> k (name, t)
    _ -> do
      name <- Renamed (prefixOfType t) <$> fresh
      first (KLet (KDecl (name, t) e)) <$> k (name, t)

bindExprs :: [Norm (KExpr, KType)] -> ([KBinder] -> Norm (KExpr, KType)) -> Norm (KExpr, KType)
bindExprs ms k = go [] ms
 where
  go bs [] = k $ reverse bs
  go bs (m:ms) = bindExpr m $ \b -> go (b:bs) ms 

normExpr :: AnnExpr -> Norm (KExpr, KType)
normExpr (AVar name _) =
  (,) (KVar name) <$> asks (fromJust . M.lookup name)
normExpr (AValue (BoolValue b)) =
  return (KValue $ IntValue $ if b then 1 else 0, IntType)
normExpr (AValue val) =
  return (KValue val, toKType $ typeOf val)
normExpr (AIf (AOp (Cmp cmp) [e1, e2]) e3 e4) =
  bindExpr (normExpr e1) $ \(name1, _) ->
    bindExpr (normExpr e2) $ \(name2, _) -> do
      (e3', t) <- normExpr e3
      (e4', _) <- normExpr e4
      return (KIf cmp name1 name2 e3' e4', t)
normExpr (AIf e1 e2 e3) =
  normExpr $ AIf (AOp (Cmp Eq) [e1, AValue (BoolValue True)]) e2 e3
normExpr (ALet (ATupleDecl bs e1) e2) = do
  bindExpr (normExpr e1) $ \(name, _) -> do
    let bs' = map (second $ \(TypeScheme _ t) -> toKType t) bs
        ds = zipWith (\b i -> KDecl b (KProj i name)) bs' [1..]
    first (flip (foldr KLet) ds) <$> withBind bs' (normExpr e2)
normExpr (ALet d e) = do
  (d', bs) <- normDecl d
  first (KLet d') <$> withBind bs (normExpr e)
normExpr (AMatch e alts) =
  bindExpr (normExpr e) $ \(name, _) ->
    bimap (KMatch name) head . unzip <$> mapM normAlt alts
normExpr e@(AFun _ _) = do
  let (bs, e') = first (map (second toKType)) $ foldFunction e
  (e'', t) <- withBind bs $ normExpr e'
  name <- Renamed "f" <$> fresh 
  let t' = foldr FunType t $ map snd bs
  return (KLet (KFunDecl (name, t') bs e'') (KVar name), t')
normExpr e@(AApply _ _) = do
  let (e', args) = foldApply e
  bindExpr (normExpr e') $ \(name, t) ->
    bindExprs (map normExpr args) $ \bs ->
      return (KApply name (map fst bs), returnType t (length args))
normExpr (AOp op es) = do
  let t = case op of
           Cmp _ -> IntType
           Arith _ -> IntType
           FArith _ -> FloatType
  bindExprs (map normExpr es) $ \bs ->
    return (KOp op (map fst bs), t)
normExpr (ACon con tcon targs es) =
  bindExprs (map normExpr es) $ \bs -> do
    let targs' = map toKType targs
    return (KCon con (map fst bs), DataType targs' tcon)
normExpr (ATuple es) =
  bindExprs (map normExpr es) $ \bs ->
    return $ bimap KTuple TupleType $ unzip bs

normDecl :: AnnDecl -> Norm (KDecl, [KBinder])
normDecl (ARecDecl (name, TypeScheme [] t) e) = do
  let b = (name, toKType t)
  let (bs, e') = first (map (second toKType)) $ foldFunction e
  bimap (KFunDecl b bs) (const [b]) <$> withBind (b:bs) (normExpr e')
normDecl (ADecl (name, TypeScheme [] t) e) = do
  let b = (name, toKType t)
  let (bs, e') = first (map (second toKType)) $ foldFunction e
  case bs of
    [] -> bimap (KDecl b) (const [b]) <$> withBind bs (normExpr e') 
    _ -> bimap (KFunDecl b bs) (const [b]) <$> withBind bs (normExpr e')

normAlt :: AnnAlt -> Norm (KAlt, KType)
normAlt (AConCase con _ _ bs e) = do
  let bs' = map (second toKType) bs
  first (KConCase con bs') <$> withBind bs' (normExpr e)
normAlt (ADefaultCase e) =
  first KDefaultCase <$> normExpr e

foldFunction :: AnnExpr -> ([MonoBinder], AnnExpr) 
foldFunction (AFun b e) = first (b:) $ foldFunction e
foldFunction e = ([], e)

foldApply :: AnnExpr -> (AnnExpr, [AnnExpr])
foldApply = second reverse . go
 where
  go (AApply e arg) = second (arg:) $ go e
  go e = (e, [])
