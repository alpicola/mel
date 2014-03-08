module K.AST where

import Data.Set (Set)
import qualified Data.Set as S

import Frontend.AST
import Frontend.Types
import Frontend.Values
import Frontend.Primitives

import Internal

-- K-normal form AST

type KProgram = ([DataTypeDecl], KExpr)

data KExpr = KVar Name
           | KValue Value
           | KIf CmpOp Name Name KExpr KExpr
           | KLet KDecl KExpr
           | KMatch Name [KAlt]
           | KApply Name [Name]
           | KOp PrimOp [Name]
           | KCon Name Name [Type] [Name]
           | KTuple [Name]
           | KProj Int Name
           deriving Show

data KDecl = KFunDecl MonoBinder [MonoBinder] KExpr
           | KDecl MonoBinder KExpr
           deriving Show

data KAlt = KConCase Name Name [Type] [MonoBinder] KExpr
          | KDefaultCase KExpr
          deriving Show

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
freeVars (KMatch n alts) =
  S.insert n $ S.unions $ map f alts
 where
  f (KConCase _ _ _ bs e) =
    S.difference (freeVars e) $ S.fromList $ map fst bs
  f (KDefaultCase e) = freeVars e
freeVars (KApply n ns) = S.fromList (n:ns)
freeVars (KCon _ _ _ ns) = S.fromList ns
freeVars (KTuple ns) = S.fromList ns
freeVars (KProj _ n) = S.singleton n

flattenLet :: KExpr -> KExpr
flattenLet (KIf cmp n1 n2 e1 e2) =
  KIf cmp n1 n2 (flattenLet e1) (flattenLet e2)
flattenLet (KMatch n alts) =
  KMatch n $ map f alts
 where
  f (KConCase con tcon ts bs e) =
    KConCase con tcon ts bs $ flattenLet e
  f (KDefaultCase e) =
    KDefaultCase $ flattenLet e
flattenLet (KLet (KDecl b e1) e2) = go $ flattenLet e1
 where
  go (KLet d e) = KLet d $ go e
  go e = KLet (KDecl b e) $ flattenLet e2
flattenLet e = e

sideEffect :: KExpr -> Bool
sideEffect (KIf _ _ _ e1 e2) = sideEffect e1 || sideEffect e2
sideEffect (KLet (KDecl _ e1) e2) = sideEffect e1 || sideEffect e2
sideEffect (KLet (KFunDecl _ _ _) e2) = sideEffect e2
sideEffect (KMatch _ alts) = any f alts
 where
  f (KConCase _ _ _ _ e) = sideEffect e
  f (KDefaultCase e) = sideEffect e
sideEffect (KApply _ _) = True
sideEffect _ = False 
