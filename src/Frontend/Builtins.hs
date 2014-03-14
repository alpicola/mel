module Frontend.Builtins where

import qualified Data.Map as M

import Frontend.AST
import Frontend.Types

import Internal

builtinDataTypes :: [DataTypeDecl]
builtinDataTypes =
  [ (1, Raw "list", [(Raw "[]", []), (Raw "::", [TypeVar 0, DataType [TypeVar 0] (Raw "list")])]) ]

builtinFunctions :: MonoTypeEnv
builtinFunctions = M.fromList
  [ (External "print_int", FunType IntType UnitType)
  , (External "float_of_int", FunType IntType FloatType)
  , (External "int_of_float", FunType FloatType IntType)
  ]
