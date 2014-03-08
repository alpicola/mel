module K.ConstFold (constFold) where

import Control.Applicative
import Control.Monad.Reader
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
-- TODO: deal with tuples and datatypes

type CopyEnv = Map Name Name
type ConstEnv = Map Name Value
type CF a = Reader (CopyEnv, ConstEnv) a

runCF :: (CopyEnv, ConstEnv) -> CF a -> a
runCF = flip runReader

pullName :: Name -> CF Name
pullName name = asks $ fromMaybe name . M.lookup name . fst

pullInt :: Name -> CF (Maybe Int)
pullInt name = do
  val <- asks $ fromMaybe UnitValue . M.lookup name . snd
  case val of
    IntValue i -> return $ Just i
    _ -> return Nothing

pullFloat :: Name -> CF (Maybe Float)
pullFloat name = do
  val <- asks $ fromMaybe UnitValue . M.lookup name . snd
  case val of
    FloatValue f -> return $ Just f
    _ -> return Nothing

cfExpr :: KExpr -> CF KExpr
cfExpr (KVar n) = KVar <$> pullName n
cfExpr (KValue v) = return $ KValue v
cfExpr (KIf op n1 n2 e1 e2) = do
  n1' <- pullName n1
  n2' <- pullName n2
  e1' <- cfExpr e1
  e2' <- cfExpr e2
  r1 <- fmap (evalCmpOp op) . sequence <$> mapM pullInt [n1', n2']
  r2 <- fmap (evalCmpOp op) . sequence <$> mapM pullFloat [n1', n2']
  case r1 `mplus` r2 of
    Just b -> if b then cfExpr e1 else cfExpr e2
    Nothing -> KIf op n1' n2' <$> cfExpr e1 <*> cfExpr e2
cfExpr (KLet (KFunDecl b bs e1) e2) =
  KLet . (KFunDecl b bs) <$> cfExpr e1 <*> cfExpr e2
cfExpr (KLet (KDecl (n, t) e1) e2) = do
  e1' <- cfExpr e1
  case e1' of
    KVar n' ->
      local (first $ M.insert n n') $ cfExpr e2
    KValue v ->
      KLet (KDecl (n, t) e1') <$> local (second $ M.insert n v) (cfExpr e2)
    _ ->
      KLet (KDecl (n, t) e1') <$> cfExpr e2
cfExpr (KMatch n alts) =
  KMatch <$> pullName n <*> mapM cfAlt alts
cfExpr (KMatch1 n alt) =
  KMatch1 <$> pullName n <*> cfAlt alt
cfExpr (KApply n ns) =
  KApply <$> pullName n <*> mapM pullName ns
cfExpr (KOp (Cmp op) ns) = do
  ns' <- mapM pullName ns
  r1 <- fmap (evalCmpOp op) . sequence <$> mapM pullInt ns'
  r2 <- fmap (evalCmpOp op) . sequence <$> mapM pullFloat ns'
  case r1 `mplus` r2 of
    Just b -> return $ KValue $ IntValue $ if b then 1 else 0 
    Nothing -> return $ KOp (Cmp op) ns'
cfExpr (KOp (Arith op) ns) = do
  ns' <- mapM pullName ns
  r <- fmap (evalArithOp op) . sequence <$> mapM pullInt ns'
  case r of
    Just i -> return $ KValue $ IntValue i
    Nothing -> return $ KOp (Arith op) ns'
cfExpr (KOp (FArith op) ns) = do
  ns' <- mapM pullName ns
  r <- fmap (evalFArithOp op) . sequence <$> mapM pullFloat ns'
  case r of
    Just f -> return $ KValue $ FloatValue f
    Nothing -> return $ KOp (FArith op) ns'
cfExpr (KCon con ns) =
  KCon con <$> mapM pullName ns

cfAlt :: KAlt -> CF KAlt
cfAlt (KConCase con bs e) =
  KConCase con bs <$> cfExpr e
cfAlt (KDefaultCase e) =
  KDefaultCase <$> cfExpr e
