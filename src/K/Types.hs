module K.Types
  ( module K.Types
  , module Frontend.Types
  ) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Frontend.Types

import Internal

type KType = Type
type KTypeEnv = Map Name KType

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
