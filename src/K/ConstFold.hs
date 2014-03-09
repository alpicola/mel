module K.ConstFold (constFold) where

import Control.Applicative
import Control.Monad.Reader
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Bifunctor

import Frontend.AST
import Frontend.Types
import Frontend.Values
import Frontend.Primitives
import K.AST

import Internal

constFold :: KProgram -> KProgram
constFold = second $ runCF (M.empty, M.empty) . cfExpr

-- Do constant folding

type CopyEnv = Map Name Name
type ConstEnv = Map Name (Either Value (Int, [Name]))
type CF a = Reader (CopyEnv, ConstEnv) a

runCF :: (CopyEnv, ConstEnv) -> CF a -> a
runCF = flip runReader

withSubst :: [(Name, Name)] -> CF a -> CF a
withSubst s = local $ first $ flip (foldr $ uncurry M.insert) s

withConstValue :: Name -> Value -> CF a -> CF a
withConstValue n v = local $ second $ M.insert n (Left v)

withConstData :: Name -> (Int, [Name]) -> CF a -> CF a
withConstData n d = local $ second $ M.insert n (Right d)

pullName :: Name -> CF Name
pullName name = asks $ fromMaybe name . M.lookup name . fst

getInt :: Name -> CF (Maybe Int)
getInt name = do
  m <- asks $ M.lookup name . snd
  return $ m >>= \val ->
    case val of
      Left (IntValue i) -> Just i
      _ -> Nothing

getFloat :: Name -> CF (Maybe Float)
getFloat name = do
  m <- asks $ M.lookup name . snd
  return $ m >>= \val ->
    case val of
      Left (FloatValue f) -> Just f
      _ -> Nothing

getData :: Name -> CF (Maybe (Int, [Name]))
getData name = do
  m <- asks $ M.lookup name . snd
  return $ m >>= \val ->
    case val of
      Right d -> Just d
      _ -> Nothing

cfExpr :: KExpr -> CF KExpr
cfExpr (KVar n) = KVar <$> pullName n
cfExpr (KValue v) = return $ KValue v
cfExpr (KIf op n1 n2 e1 e2) = do
  n1' <- pullName n1
  n2' <- pullName n2
  e1' <- cfExpr e1
  e2' <- cfExpr e2
  r1 <- fmap (evalCmpOp op) . sequence <$> mapM getInt [n1', n2']
  r2 <- fmap (evalCmpOp op) . sequence <$> mapM getFloat [n1', n2']
  case r1 `mplus` r2 of
    Just b -> if b then cfExpr e1 else cfExpr e2
    Nothing -> KIf op n1' n2' <$> cfExpr e1 <*> cfExpr e2
cfExpr (KLet (KFunDecl b bs e1) e2) =
  KLet . (KFunDecl b bs) <$> cfExpr e1 <*> cfExpr e2
cfExpr (KLet (KDecl (n, t) e1) e2) = do
  e1' <- cfExpr e1
  case e1' of
    KVar n' ->
      withSubst [(n, n')] $ cfExpr e2
    KValue v ->
      KLet (KDecl (n, t) e1') <$> withConstValue n v (cfExpr e2)
    KCon con ns ->
      KLet (KDecl (n, t) e1') <$> withConstData n (con, ns) (cfExpr e2)
    KTuple ns ->
      KLet (KDecl (n, t) e1') <$> withConstData n (0, ns) (cfExpr e2)
    _ ->
      KLet (KDecl (n, t) e1') <$> cfExpr e2
cfExpr (KLet (KTupleDecl bs n) e) = do
  n' <- pullName n
  m <- getData n
  case m of
    Nothing -> KLet (KTupleDecl bs n') <$> cfExpr e
    Just (_, ns) -> withSubst (zip (map fst bs) ns) $ cfExpr e
cfExpr (KMatch n alts) = do
  n' <- pullName n
  m <- getData n
  case m of
    Nothing -> KMatch n' <$> mapM cfAlt alts 
    Just (con, ns) -> cfAlt' ns $ fromJust $ find (isMatch con) alts
cfExpr (KApply n ns) =
  KApply <$> pullName n <*> mapM pullName ns
cfExpr (KOp (Cmp op) ns) = do
  ns' <- mapM pullName ns
  r1 <- fmap (evalCmpOp op) . sequence <$> mapM getInt ns'
  r2 <- fmap (evalCmpOp op) . sequence <$> mapM getFloat ns'
  case r1 `mplus` r2 of
    Just b -> return $ KValue $ IntValue $ if b then 1 else 0 
    Nothing -> return $ KOp (Cmp op) ns'
cfExpr (KOp (Arith op) ns) = do
  ns' <- mapM pullName ns
  r <- fmap (evalArithOp op) . sequence <$> mapM getInt ns'
  case r of
    Just i -> return $ KValue $ IntValue i
    Nothing -> return $ KOp (Arith op) ns'
cfExpr (KOp (FArith op) ns) = do
  ns' <- mapM pullName ns
  r <- fmap (evalFArithOp op) . sequence <$> mapM getFloat ns'
  case r of
    Just f -> return $ KValue $ FloatValue f
    Nothing -> return $ KOp (FArith op) ns'
cfExpr (KCon con ns) =
  KCon con <$> mapM pullName ns
cfExpr (KTuple ns) =
  KTuple <$> mapM pullName ns

cfAlt :: KAlt -> CF KAlt
cfAlt (KConCase con bs e) =
  KConCase con bs <$> cfExpr e
cfAlt (KDefaultCase e) =
  KDefaultCase <$> cfExpr e

cfAlt' :: [Name] -> KAlt -> CF KExpr
cfAlt' ns (KConCase _ bs e) =
  withSubst (zip (map fst bs) ns) $ cfExpr e
cfAlt' _ (KDefaultCase e) = cfExpr e

isMatch :: Int -> KAlt -> Bool
isMatch con (KConCase con' _ _) = con == con'
isMatch _ (KDefaultCase _) = True
