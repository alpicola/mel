module K.AST where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Frontend.AST
import Frontend.Types
import Frontend.Values
import Frontend.Primitives

import K.Types

import Internal

-- K-normal form AST

type KDataTypeTable = Map Name Int
type KProgram = (KDataTypeTable, KExpr)

type KBinder = (Name, KType)

data KExpr = KVar Name
           | KValue Value
           | KIf CmpOp Name Name KExpr KExpr
           | KLet KDecl KExpr
           | KMatch Name [KAlt]
           | KApply Name [Name]
           | KOp PrimOp [Name]
           | KCon Int [Name]
           | KTuple [Name]
           deriving (Eq, Show)

data KDecl = KFunDecl KBinder [KBinder] KExpr
           | KDecl KBinder KExpr
           | KTupleDecl [KBinder] Name
           deriving (Eq, Show)

data KAlt = KConCase Int [KBinder] KExpr
          | KDefaultCase KExpr
          deriving (Eq, Show)

freeVars :: KExpr -> Set Name
freeVars (KVar n) = S.singleton n
freeVars (KValue _) = S.empty
freeVars (KIf _ n1 n2 e1 e2) =
  S.insert n1 $ S.insert n2 $ S.union (freeVars e1) (freeVars e2)
freeVars (KLet (KFunDecl (n, _) bs e1) e2) =
  let vs = S.difference (freeVars e1) $ S.fromList $ map fst bs
  in S.delete n $ S.union vs (freeVars e2)
freeVars (KLet (KDecl (n, _) e1) e2) =
  S.union (freeVars e1) $ S.delete n (freeVars e2)
freeVars (KLet (KTupleDecl bs n) e2) =
  S.insert n $ S.difference (freeVars e2) $ S.fromList $ map fst bs
freeVars (KMatch n alts) =
  S.insert n $ S.unions $ map f alts
 where
  f (KConCase _ bs e) =
    S.difference (freeVars e) $ S.fromList $ map fst bs
  f (KDefaultCase e) = freeVars e
freeVars (KApply n ns) = S.fromList (n:ns)
freeVars (KOp _ ns) = S.fromList ns
freeVars (KCon _ ns) = S.fromList ns
freeVars (KTuple ns) = S.fromList ns

sizeOf :: KExpr -> Int
sizeOf (KIf _ _ _ e1 e2) = 1 + sizeOf e1 + sizeOf e2
sizeOf (KLet (KFunDecl _ _ e1) e2) = 1 + sizeOf e1 + sizeOf e2
sizeOf (KLet (KDecl _ e1) e2) = 1 + sizeOf e1 + sizeOf e2
sizeOf (KLet (KTupleDecl _ _) e) = 1 + sizeOf e
sizeOf (KMatch _ alts) = 1 + sum (map f alts)
 where
  f (KConCase _ _ e) = sizeOf e
  f (KDefaultCase e) = sizeOf e
sizeOf _ = 1

hasSideEffects :: KExpr -> Bool
hasSideEffects (KIf _ _ _ e1 e2) = hasSideEffects e1 || hasSideEffects e2
hasSideEffects (KLet (KFunDecl _ _ _) e2) = hasSideEffects e2
hasSideEffects (KLet (KDecl _ e1) e2) = hasSideEffects e1 || hasSideEffects e2
hasSideEffects (KLet (KTupleDecl _ _) e) = hasSideEffects e
hasSideEffects (KMatch _ alts) = any f alts
 where
  f (KConCase _ _ e) = hasSideEffects e
  f (KDefaultCase e) = hasSideEffects e
hasSideEffects (KApply _ _) = True
hasSideEffects _ = False 

flattenLet :: KExpr -> KExpr
flattenLet (KIf cmp n1 n2 e1 e2) =
  KIf cmp n1 n2 (flattenLet e1) (flattenLet e2)
flattenLet (KMatch n alts) =
  KMatch n $ map f alts
 where
  f (KConCase con bs e) =
    KConCase con bs $ flattenLet e
  f (KDefaultCase e) =
    KDefaultCase $ flattenLet e
flattenLet (KLet (KFunDecl b bs e1) e2) =
  KLet (KFunDecl b bs (flattenLet e1)) (flattenLet e2)
flattenLet (KLet (KDecl b e1) e2) = go $ flattenLet e1
 where
  go (KLet d e) = KLet d $ go e
  go e = KLet (KDecl b e) $ flattenLet e2
flattenLet (KLet (KTupleDecl bs n) e) =
  KLet (KTupleDecl bs n) (flattenLet e)
flattenLet e = e
