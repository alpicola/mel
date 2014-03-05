module Frontend.Builtins where

import qualified Data.Map as M

import Frontend.Types

import Internal

builtinDataTypes :: [DataTypeDecl]
builtinDataTypes =
  [ (1, Raw "list", [(Raw "[]", []), (Raw "::", [TypeVar 0, DataType [TypeVar 0] (Raw "list")])]) ]

builtinFunctons :: MonoTypeEnv
builtinFunctons = M.fromList
  [ (External "print_int", FunType IntType UnitType) ] 
