module Frontend.AST where

import Frontend.Types
import Internal

type Binder a = Maybe a

data Value = IntValue Int
           | FloatValue Double
           | BoolValue Bool
           | UnitValue

instance Show Value where
  show (IntValue i) = show i
  show (FloatValue f) = show f
  show (BoolValue True) = "true"
  show (BoolValue False) = "false"
  show UnitValue = "unit"

data PrimOp = Eq | Neq | Lt | Le | Gt | Ge
            | Plus | Minus | Times | Div
            | FPlus | FMinus | FTimes | FDiv
            deriving (Show, Eq, Ord)

isCompOp, isArithOp, isFArithOp :: PrimOp -> Bool
isCompOp op = op <= Ge
isArithOp op = Plus <= op && op <= Div
isFArithOp op = FPlus <= op && op <= FDiv

-- Raw AST

type MLProgram = ([MLTypeDecl], [MLDecl])

type MLTypeDecl = ([Name], Name, [(Name, [MLType])])

type MLBinder = Binder Name

data MLDecl = MLRecDecl MLBinder MLExpr
            | MLDecl MLBinder MLExpr

data MLType = MLTypeVar Name
            | MLTypeCon [MLType] Name
            | MLFunType MLType MLType
            | MLTupleType [MLType]

data MLExpr = MLVar Name
            | MLValue Value
            | MLIf MLExpr MLExpr MLExpr
            | MLLet MLDecl MLExpr
            | MLMatch MLExpr [MLAlt]
            | MLFun MLBinder MLExpr
            | MLApply MLExpr MLExpr
            | MLOp PrimOp [MLExpr]
            | MLCon Name [MLExpr]
            | MLTuple [MLExpr]

data MLAlt = MLConCase Name [MLBinder] MLExpr
           | MLDefaultCase MLExpr

-- Annotated AST

type AnnProgram = ([DataType], AnnExpr)

type AnnBinder = Binder (Name, Type)
type AnnPolyBinder = Binder (Name, TypeScheme)

data AnnDecl = ARecDecl AnnPolyBinder AnnExpr
             | ADecl AnnPolyBinder AnnExpr
             deriving Show

data AnnExpr = AVar Name Type
             | AValue Value
             | AIf AnnExpr AnnExpr AnnExpr
             | ALet AnnDecl AnnExpr
             | AMatch AnnExpr [AnnAlt] Type
             | AFun AnnBinder AnnExpr
             | AApply AnnExpr AnnExpr
             | AOp PrimOp [AnnExpr]
             | ACon Name [AnnExpr] Type [Type]
             | ATuple [AnnExpr]
             deriving Show

data AnnAlt = AConCase Name [AnnBinder] AnnExpr
            | ADefaultCase AnnExpr
            deriving Show
