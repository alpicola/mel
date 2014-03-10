module K.UnusedElim (unusedElim) where

import Control.Applicative
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.Bifunctor
import Data.Bitraversable

import K.AST

import Internal

-- Eliminate unused declarations

unusedElim :: KProgram -> KProgram
unusedElim = second $ runUE . ueExpr

type UE a = State (Set Name) a

runUE :: UE a -> a
runUE = flip evalState S.empty 

useOf :: Name -> UE Bool
useOf name = gets $ S.member name

use :: Name -> UE ()
use name = modify $ S.insert name

ueBinder :: KBinder -> UE KBinder
ueBinder = flip bimapM return $ \n -> do
  isUsed <- useOf n
  return $ if isUsed then n else Erased

ueExpr :: KExpr -> UE KExpr
ueExpr (KVar n) = KVar n <$ use n
ueExpr (KValue val) = return $ KValue val
ueExpr (KIf cmp n1 n2 e1 e2) = do
  use n1
  use n2
  KIf cmp n1 n2 <$> ueExpr e1 <*> ueExpr e2
ueExpr (KMatch n alts) = do
  use n
  KMatch n <$> mapM ueAlt alts
ueExpr (KLet (KFunDecl b bs e1) e2) = do
  e1' <- ueExpr e1
  e2' <- ueExpr e2
  b' <- ueBinder b
  bs' <- mapM ueBinder bs
  if isErased (fst b')
    then return e2'
    else return $ KLet (KFunDecl b' bs' e1') e2'
ueExpr (KLet (KDecl b e1) e2) = do
  e1' <- ueExpr e1
  e2' <- ueExpr e2
  b' <- ueBinder b
  if isErased (fst b') && not (hasSideEffects e1')
    then return e2'
    else  return $ KLet (KDecl b' e1') e2'
ueExpr (KLet (KTupleDecl bs n) e) = do
  use n
  e' <- ueExpr e
  bs' <- mapM ueBinder bs
  if all (isErased . fst) bs'
    then return e'
    else return $ KLet (KTupleDecl bs' n) e'
ueExpr (KApply n ns) = KApply n ns <$ mapM_ use (n:ns)
ueExpr (KOp op ns) = KOp op ns <$ mapM_ use ns
ueExpr (KCon con ns) = KCon con ns <$ mapM_ use ns
ueExpr (KTuple ns) = KTuple ns <$ mapM_ use ns

ueAlt :: KAlt -> UE KAlt
ueAlt (KConCase con bs e) = do
  e' <- ueExpr e
  bs' <- mapM ueBinder bs
  return $ KConCase con bs' e'
ueAlt (KDefaultCase e) =
  KDefaultCase <$> ueExpr e
