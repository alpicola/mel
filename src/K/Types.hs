module K.Types where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Frontend.Types

import Internal

data KType = KIntType
           | KFloatType
           | KUnitType
           | KFunType KType KType
           | KDataType Name [KType]
           deriving (Eq, Ord)

type KTypeEnv = Map Name KType

instance Show KType where
 show = showKType

showKType :: KType -> String
showKType t = f 0 t
 where
  f _ KIntType = "int"
  f _ KFloatType = "float"
  f _ KUnitType = "unit"
  f p (KFunType t1 t2) =
    let s = f 1 t1 ++ " -> " ++ f 0 t2
    in if p > 0 then "(" ++ s ++ ")" else s 
  f p (KDataType n []) = show n
  f p (KDataType n [t]) =
    let s = f 2 t ++ " " ++ show n
    in if p > 1 then "(" ++ s ++ ")" else s
  f p (KDataType n ts) =
    let s = "(" ++ intercalate ", " (map (f 0) ts) ++ ") " ++ show n
    in if p > 1 then "(" ++ s ++ ")" else s

returnType :: KType -> Int -> KType
returnType t 0 = t
returnType (KFunType _ t) i = returnType t (i - 1)
returnType t _ = t

prefixOfType :: KType -> String
prefixOfType (KFunType _ _) = "f"
prefixOfType (KDataType n _) = take 1 $ show n
prefixOfType t = take 1 $ show t

toKType :: Type -> KType
toKType (TypeVar _) = KUnitType
toKType IntType = KIntType
toKType FloatType = KFloatType
toKType BoolType = KIntType
toKType UnitType = KUnitType
toKType (FunType t1 t2) = KFunType (toKType t1) (toKType t2)
toKType (TupleType ts) = KDataType (tupleTypeName (length ts)) (map toKType ts) 
toKType (DataType ts tcon) = KDataType tcon (map toKType ts)

tupleTypeName :: Int -> Name
tupleTypeName i = Special $ "tuple" ++ show i
