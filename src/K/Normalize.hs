{-# LANGUAGE TupleSections #-}
module K.Normalize (normalize) where

import Control.Applicative
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map as M
import Data.Bifunctor
import Data.Bitraversable

import Frontend.AST
import Frontend.Types
import Frontend.Values
import Frontend.Builtins
import Frontend.Primitives
import K.AST

import Internal

normalize :: AnnProgram -> Fresh KProgram
normalize = bimapM return $ fmap (flattenLet . snd) . runNorm builtinFunctions . normExpr

-- K-normalization

type Norm a = ReaderT MonoTypeEnv Fresh a

runNorm :: MonoTypeEnv -> Norm a -> Fresh a
runNorm env = flip runReaderT env

withBind :: [MonoBinder] -> Norm a -> Norm a
withBind bs = local $ flip (foldr $ uncurry M.insert) bs'
 where
  bs' = filter (not . isErased . fst) bs

bindExpr :: Norm (Type, KExpr) -> (Name -> Norm (Type, KExpr)) -> Norm (Type, KExpr)
bindExpr m k = do
  (t, e) <- m
  case e of
    (KVar name) -> k name 
    _ -> do
      name <- Renamed (prefixOfType t) <$> fresh
      second (KLet (KDecl (name, t) e)) <$> k name

bindExprs :: [Norm (Type, KExpr)] -> ([Name] -> Norm (Type, KExpr)) -> Norm (Type, KExpr)
bindExprs ms k = go [] ms
 where
  go names [] = k $ reverse names
  go names (m:ms) = bindExpr m $ \name -> go (name:names) ms 

normExpr :: AnnExpr -> Norm (Type, KExpr)
normExpr (AVar name _) =
  (, KVar name) <$> asks (fromJust . M.lookup name) 
normExpr (AValue (BoolValue b)) =
  return (IntType, KValue $ IntValue $ if b then 1 else 0)
normExpr (AValue val) =
  return (typeOf val, KValue val)
normExpr (AIf (AOp (Cmp cmp) [e1, e2]) e3 e4) =
  bindExpr (normExpr e1) $ \name1 ->
    bindExpr (normExpr e2) $ \name2 -> do
      (t, e3') <- normExpr e3
      (_, e4') <- normExpr e4
      return (t, KIf cmp name1 name2 e3' e4')
normExpr (AIf e1 e2 e3) =
  normExpr $ AIf (AOp (Cmp Eq) [e1, AValue (BoolValue True)]) e2 e3
normExpr (ALet (ATupleDecl bs e1) e2) = do
  let bs' = map (second $ \(TypeScheme _ t) -> t) bs
  bindExpr (withBind bs' $ normExpr e1) $ \name -> do
    let f (b, i) e = KLet (KDecl b (KProj i name)) e
    second (flip (foldr f) $ zip bs' [0..]) <$> normExpr e2
normExpr (ALet d e) = do
  (bs, d') <- normDecl d
  second (KLet d') <$> withBind bs (normExpr e)
normExpr (AMatch e alts) =
  bindExpr (normExpr e) $ \name ->
    bimap head (KMatch name) . unzip <$> mapM normAlt alts
normExpr e@(AFun _ _) = do
  let (bs, e') = foldFunction e
  (t, e'') <- withBind bs $ normExpr e'
  name <- Renamed "f" <$> fresh 
  let t' = foldr FunType t $ map snd bs
  return (t', KLet (KFunDecl (name, t') bs e'') (KVar name))
normExpr e@(AApply _ _) = do
  let (e', args) = foldApply e
  (t, e'') <- normExpr e'
  let t' = returnType t $ length args
  bindExpr (return (t, e'')) $ \name ->
    bindExprs (map normExpr args) $ \names ->
      return (t', KApply name names)
normExpr (AOp op es) = do
  let t = case op of
           Cmp _ -> IntType
           Arith _ -> IntType
           FArith _ -> FloatType
  bindExprs (map normExpr es) $ \names ->
    return (t, KOp op names)
normExpr (ACon con tcon targs es) =
  bindExprs (map normExpr es) $ \names ->
    return (DataType targs tcon, KCon con tcon targs names)
normExpr (ATuple es) = do
  (ts, es') <- unzip <$> mapM normExpr es
  bindExprs (map return $ zip ts es') $ \names ->
    return (TupleType ts, KTuple names)

normDecl :: AnnDecl -> Norm ([MonoBinder], KDecl)
normDecl (ARecDecl (name, TypeScheme [] t) e) = do
  let b = (name, t)
  let (bs, e') = foldFunction e
  (_, e'') <- withBind (b:bs) $ normExpr e'
  return ([b], KFunDecl b bs e'')
normDecl (ADecl (name, TypeScheme [] t) e) = do
  let b = (name, t)
  let (bs, e') = foldFunction e
  (_, e'') <- withBind bs $ normExpr e'
  case bs of
    [] -> return ([b], KDecl b e'')
    _ -> return ([b], KFunDecl b bs e'')

normAlt :: AnnAlt -> Norm (Type, KAlt)
normAlt (AConCase con tcon targs bs e) = do
  second (KConCase con tcon targs bs) <$> withBind bs (normExpr e)
normAlt (ADefaultCase e) =
  second KDefaultCase <$> normExpr e

foldFunction :: AnnExpr -> ([MonoBinder], AnnExpr) 
foldFunction (AFun b e) = first (b:) $ foldFunction e
foldFunction e = ([], e)

foldApply :: AnnExpr -> (AnnExpr, [AnnExpr])
foldApply = second reverse . go
 where
  go (AApply e arg) = second (arg:) $ go e
  go e = (e, [])
