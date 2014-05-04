module Backend.AST
  ( module Backend.AST
  , module Frontend.Types
  ) where

import Data.Map (Map)
import qualified Data.Map as M

import Frontend.Types
import Frontend.Values
import Frontend.Primitives

import Internal

-- SSA-like lowlevel IR

type Label = Name

type LType = Type
type LTypeEnv = Map Name LType

type LBinder = (Name, LType)

type LProgram = [LDecl]

data LDecl = LFunDecl Type Name [LBinder] [LBlock]
           -- | LConstDecl

data LBlock = LBlock Label [LBinder] [LStatement] LTransfur

type LStatement = (LBinder, LInst)

data LInst = LVar Name
           | LValue Value
           | LCall Name [Name]
           | LOp PrimOp [Name]
           | LCon Int [Name]
           | LTuple [Name]
           | LProj Int Name
           | LExtCall String [Name]
           | LCast LType Name

data LTransfur = LReturn Name
               | LIf CmpOp Name Name Label Label
               | LMatch Name [(Int, Label)] (Maybe Label)
               | LGoto Label [Name]
               | LTailCall Name [Name]
