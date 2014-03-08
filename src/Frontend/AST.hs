module Frontend.AST where

import Data.Bifunctor

import Frontend.Types
import Frontend.Values
import Frontend.Primitives
import Internal

-- Raw AST

type Binder = Name

type MLProgram = ([MLTypeDecl], [MLDecl])

type MLTypeDecl = ([Name], Name, [(Name, [MLType])])

data MLType = MLTypeVar Name
            | MLTypeCon [MLType] Name
            | MLFunType MLType MLType
            | MLTupleType [MLType]

data MLExpr = MLVar Name
            | MLValue Value
            | MLIf MLExpr MLExpr MLExpr
            | MLLet MLDecl MLExpr
            | MLMatch MLExpr [MLAlt]
            | MLFun Binder MLExpr
            | MLApply MLExpr MLExpr
            | MLOp PrimOp [MLExpr]
            | MLCon Name [MLExpr]
            | MLTuple [MLExpr]

data MLDecl = MLRecDecl Binder MLExpr
            | MLDecl Binder MLExpr
            | MLUnitDecl MLExpr
            | MLTupleDecl [Binder] MLExpr

data MLAlt = MLConCase Name [Binder] MLExpr
           | MLDefaultCase MLExpr

-- Annotated AST

type MonoBinder = (Name, Type)
type PolyBinder = (Name, TypeScheme)

type AnnProgram = ([DataTypeDecl], AnnExpr)

data AnnExpr = AVar Name [Type]
             | AValue Value
             | AIf AnnExpr AnnExpr AnnExpr
             | ALet AnnDecl AnnExpr
             | AMatch AnnExpr [AnnAlt]
             | AFun MonoBinder AnnExpr
             | AApply AnnExpr AnnExpr
             | AOp PrimOp [AnnExpr]
             | ACon Name Name [Type] [AnnExpr]
             | ATuple [AnnExpr]
             deriving Show

data AnnDecl = ARecDecl PolyBinder AnnExpr
             | ADecl PolyBinder AnnExpr
             | ATupleDecl [PolyBinder] AnnExpr
             deriving Show

data AnnAlt = AConCase Name Name [Type] [MonoBinder] AnnExpr
            | ADefaultCase AnnExpr
            deriving Show

isValue :: AnnExpr -> Bool
isValue (AVar _ _) = True
isValue (AValue _) = True
isValue (AFun _ _) = True
isValue (ACon _ _ _ _) = True
isValue (ATuple _) = True
isValue _ = False

substBinder :: TypeLike t => TypeSubst -> (Name, t) -> (Name, t)
substBinder s = second $ subst s

substExpr :: TypeSubst -> AnnExpr -> AnnExpr
substExpr s (AVar n ts) =
  AVar n (map (subst s) ts)
substExpr s (AIf e1 e2 e3) =
  AIf (substExpr s e1) (substExpr s e2) (substExpr s e3)
substExpr s (ALet d e) =
  ALet (substDecl s d) (substExpr s e) 
substExpr s (AMatch e alts) =
  AMatch (substExpr s e) (map (substAlt s) alts)
substExpr s (AFun b e) =
  AFun (substBinder s b) (substExpr s e) 
substExpr s (AApply e1 e2) =
  AApply (substExpr s e1) (substExpr s e2)
substExpr s (AOp op es) =
  AOp op (map (substExpr s) es)
substExpr s (ACon con tcon ts es) =
  ACon con tcon (map (subst s) ts) (map (substExpr s) es)
substExpr s (ATuple es) =
  ATuple (map (substExpr s) es)
substExpr s e = e

substDecl :: TypeSubst -> AnnDecl -> AnnDecl
substDecl s (ARecDecl b e) =
  ARecDecl (substBinder s b) (substExpr s e)
substDecl s (ADecl b e) =
  ADecl (substBinder s b) (substExpr s e)
substDecl s (ATupleDecl bs e) =
  ATupleDecl (map (substBinder s) bs) (substExpr s e)

substAlt :: TypeSubst -> AnnAlt -> AnnAlt
substAlt s (AConCase con tcon ts bs e) =
  AConCase con tcon (map (subst s) ts) (map (substBinder s) bs) (substExpr s e)
substAlt s (ADefaultCase e) =
  ADefaultCase (substExpr s e)
