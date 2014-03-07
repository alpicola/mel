module Frontend.AST where

import Data.Bifunctor

import Frontend.Types
import Internal

data Value = IntValue Int
           | FloatValue Double
           | BoolValue Bool
           | UnitValue

instance Show Value where
  show (IntValue i) = show i
  show (FloatValue f) = show f
  show (BoolValue True) = "true"
  show (BoolValue False) = "false"
  show UnitValue = "()"

typeOf :: Value -> Type
typeOf (IntValue _) = IntType
typeOf (FloatValue _) = FloatType
typeOf (BoolValue _) = BoolType
typeOf UnitValue = UnitType

data PrimOp = Cmp CmpOp
            | Arith ArithOp
            | FArith FArithOp 
            deriving Show

data CmpOp = Eq | Neq | Lt | Le | Gt | Ge
data ArithOp = Plus | Minus | Times | Div
data FArithOp = FPlus | FMinus | FTimes | FDiv

instance Show CmpOp where
  show Eq = "="
  show Neq = "<>"
  show Lt = "<"
  show Le = "<="
  show Gt = ">"
  show Ge = ">="

instance Show ArithOp where
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"

instance Show FArithOp where
  show FPlus = "+."
  show FMinus = "-."
  show FTimes = "*."
  show FDiv = "/."

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
             | ACon Name Type [AnnExpr]
             | ATuple [AnnExpr]
             deriving Show

data AnnDecl = ARecDecl PolyBinder AnnExpr
             | ADecl PolyBinder AnnExpr
             | ATupleDecl [PolyBinder] AnnExpr
             deriving Show

data AnnAlt = AConCase Name Type [MonoBinder] AnnExpr
            | ADefaultCase AnnExpr
            deriving Show

isValue :: AnnExpr -> Bool
isValue (AVar _ _) = True
isValue (AValue _) = True
isValue (AFun _ _) = True
isValue (ACon _ _ _) = True
isValue (ATuple _) = True
isValue _ = False

substBinder :: TypeLike t => TypeSubst -> (Name, t) -> (Name, t)
substBinder s = second $ subst s

substExpr :: TypeSubst -> AnnExpr -> AnnExpr
substExpr s (AVar n ts) = AVar n (map (subst s) ts)
substExpr s e@(AValue _) = e
substExpr s (AIf e1 e2 e3) = AIf (substExpr s e1) (substExpr s e2) (substExpr s e3)
substExpr s (ALet d e) = ALet (substDecl s d) (substExpr s e) 
substExpr s (AMatch e alts) = AMatch (substExpr s e) (map (substAlt s) alts)
substExpr s (AFun b e) = AFun (substBinder s b) (substExpr s e) 
substExpr s (AApply e1 e2) = AApply (substExpr s e1) (substExpr s e2)
substExpr s (AOp op es) = AOp op (map (substExpr s) es)
substExpr s (ACon con t es) = ACon con (subst s t) (map (substExpr s) es)
substExpr s (ATuple es) = ATuple (map (substExpr s) es)

substDecl :: TypeSubst -> AnnDecl -> AnnDecl
substDecl s (ARecDecl b e) = ARecDecl (substBinder s b) (substExpr s e)
substDecl s (ADecl b e) = ADecl (substBinder s b) (substExpr s e)
substDecl s (ATupleDecl bs e) = ATupleDecl (map (substBinder s) bs) (substExpr s e)

substAlt :: TypeSubst -> AnnAlt -> AnnAlt
substAlt s (AConCase con t bs e) = AConCase con (subst s t) (map (substBinder s) bs) (substExpr s e)
substAlt s (ADefaultCase e) = ADefaultCase (substExpr s e)
