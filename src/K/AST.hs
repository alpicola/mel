module K.AST where

import Frontend.AST
import Frontend.Types

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
           | KCon Name Type [Name]
           | KTuple [Name]
           | KProj Int Name
           deriving Show

data KDecl = KFunDecl MonoBinder [MonoBinder] KExpr
           | KDecl MonoBinder KExpr
           deriving Show

data KAlt = KConCase Name Type [MonoBinder] KExpr
          | KDefaultCase KExpr
          deriving Show

-- Flatten let-bindings

flattenExpr :: KExpr -> KExpr
flattenExpr (KIf cmp n1 n2 e1 e2) =
  KIf cmp n1 n2 (flattenExpr e1) (flattenExpr e2)
flattenExpr (KMatch n alts) =
  KMatch n $ map flattenAlt alts
flattenExpr (KLet (KDecl b e1) e2) = go $ flattenExpr e1
 where
  go (KLet d e) = KLet d $ go e
  go e = KLet (KDecl b e) e2
flattenExpr e = e

flattenAlt :: KAlt -> KAlt
flattenAlt (KConCase con t bs e) =
  KConCase con t bs $ flattenExpr e
flattenAlt (KDefaultCase e) =
  KDefaultCase $ flattenExpr e
