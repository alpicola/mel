module K.AST
  ( module K.AST
  , module Frontend.Types
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Frontend.AST
import Frontend.Types
import Frontend.Values
import Frontend.Primitives

import Internal

-- K-normal form AST

type KType = Type
type KTypeEnv = Map Name KType

type KBinder = (Name, KType)

type KProgram = ([KConDecl], [KDecl])

type KConDecl = (Name, Int)

data KExpr = KVar Name
           | KValue Value
           | KIf CmpOp Name Name KExpr KExpr
           | KLet KDecl KExpr
           | KMatch Name [KAlt]
           | KApply Name [Name]
           | KOp PrimOp [Name]
           | KCon Name [Name]
           | KTuple [Name]
           | KProj Int Name
           | KExt String KType [Name]
           deriving (Eq, Show)

data KDecl = KFunDecl KBinder [KBinder] KExpr
           | KDecl KBinder KExpr
           deriving (Eq, Show)

data KAlt = KConCase Name [KBinder] KExpr
          | KDefaultCase KExpr
          deriving (Eq, Show)

isDefaultCase :: KAlt -> Bool
isDefaultCase (KDefaultCase _) = True
isDefaultCase _ = False

bindOf :: KDecl -> KBinder
bindOf (KFunDecl b _ _) = b
bindOf (KDecl b _) = b

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
  f (KConCase _ bs e) =
    S.difference (freeVars e) $ S.fromList $ map fst bs
  f (KDefaultCase e) = freeVars e
freeVars (KApply n ns) = S.fromList (n:ns)
freeVars (KOp _ ns) = S.fromList ns
freeVars (KCon _ ns) = S.fromList ns
freeVars (KTuple ns) = S.fromList ns
freeVars (KProj _ n) = S.singleton n
freeVars (KExt _ _ ns) = S.fromList ns

sizeOf :: KExpr -> Int
sizeOf (KIf _ _ _ e1 e2) = 1 + sizeOf e1 + sizeOf e2
sizeOf (KLet (KFunDecl _ _ e1) e2) = 1 + sizeOf e1 + sizeOf e2
sizeOf (KLet (KDecl _ e1) e2) = 1 + sizeOf e1 + sizeOf e2
sizeOf (KMatch _ alts) = 1 + sum (map f alts)
 where
  f (KConCase _ _ e) = sizeOf e
  f (KDefaultCase e) = sizeOf e
sizeOf _ = 1

hasSideEffects :: KExpr -> Bool
hasSideEffects (KIf _ _ _ e1 e2) = hasSideEffects e1 || hasSideEffects e2
hasSideEffects (KLet (KFunDecl _ _ _) e2) = hasSideEffects e2
hasSideEffects (KLet (KDecl _ e1) e2) = hasSideEffects e1 || hasSideEffects e2
hasSideEffects (KMatch _ alts) = any f alts
 where
  f (KConCase _ _ e) = hasSideEffects e
  f (KDefaultCase e) = hasSideEffects e
hasSideEffects (KApply _ _) = True
hasSideEffects (KExt _ _ _) = True
hasSideEffects _ = False

flattenLet :: KExpr -> KExpr
flattenLet (KIf cmp n1 n2 e1 e2) =
  KIf cmp n1 n2 (flattenLet e1) (flattenLet e2)
flattenLet (KLet (KFunDecl b bs e1) e2) =
  KLet (KFunDecl b bs (flattenLet e1)) (flattenLet e2)
flattenLet (KLet (KDecl b e1) e2) = go $ flattenLet e1
 where
  go (KLet d e) = KLet d $ go e
  go e = KLet (KDecl b e) $ flattenLet e2
flattenLet (KMatch n alts) =
  KMatch n $ map f alts
 where
  f (KConCase con bs e) =
    KConCase con bs $ flattenLet e
  f (KDefaultCase e) =
    KDefaultCase $ flattenLet e
flattenLet e = e

-- replace bool type with int type
toKType :: Type -> KType
toKType (TypeVar _) = UnitType
toKType IntType = IntType
toKType FloatType = FloatType
toKType BoolType = IntType
toKType UnitType = UnitType
toKType (FunType t1 t2) = FunType (toKType t1) (toKType t2)
toKType (TupleType ts) = TupleType (map toKType ts) 
toKType (DataType ts tcon) = DataType (map toKType ts) tcon
