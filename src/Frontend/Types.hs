{-# LANGUAGE FlexibleInstances, UndecidableInstances, TypeSynonymInstances #-}
module Frontend.Types where

import Data.Char
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Internal

type TypeVar = Int

data Type = TypeVar TypeVar
          | IntType
          | FloatType
          | BoolType
          | UnitType
          | FunType Type Type
          | TupleType [Type]
          | DataType [Type] Name
          deriving (Eq, Ord)

data TypeScheme = TypeScheme [TypeVar] Type
type TypeSubst = Map TypeVar Type
type MonoTypeEnv = Map Name Type
type PolyTypeEnv = Map Name TypeScheme
type DataTypeDecl = (Int, Name, [(Name, [Type])])

class TypeLike t where
 fv :: t -> Set TypeVar
 subst :: TypeSubst -> t -> t

instance TypeLike Type where
  fv (TypeVar v) = S.singleton v
  fv (FunType t1 t2) = S.union (fv t1) (fv t2)
  fv (TupleType ts) = foldl S.union S.empty $ map fv ts
  fv (DataType ts _) = foldl S.union S.empty $ map fv ts
  fv _ = S.empty
  
  subst s t@(TypeVar v) = fromMaybe t $ M.lookup v s
  subst s (FunType t1 t2) = FunType (subst s t1) (subst s t2)
  subst s (TupleType ts) = TupleType (map (subst s) ts)
  subst s (DataType ts n) = DataType (map (subst s) ts) n
  subst _ t = t

instance TypeLike TypeScheme where
 fv (TypeScheme vs t) = foldr S.delete (fv t) vs
 subst s (TypeScheme vs t) = TypeScheme vs $ subst (foldr M.delete s vs) t

instance TypeLike MonoTypeEnv where
 fv = S.unions . map fv . M.elems
 subst s = M.map (subst s)

instance TypeLike PolyTypeEnv where
 fv = S.unions . map fv . M.elems
 subst s = M.map (subst s)

instance Show Type where
 show = showType []

instance Show TypeScheme where
 show (TypeScheme vs t) = showType vs t

bind :: TypeVar -> Type -> TypeSubst -> TypeSubst
bind v t s = M.insert v t $ M.map (subst $ M.singleton v t) s

showType :: [TypeVar] -> Type -> String
showType vs t = 
  case vs of
    [] -> f 0 t
    [v] -> showTypeVar v ++ "." ++ f 0 t
    _ -> "(" ++ intercalate "," (map showTypeVar vs) ++ ")." ++ f 0 t
 where
  f _ (TypeVar v) = showTypeVar v
  f _ IntType = "int"
  f _ FloatType = "float"
  f _ BoolType = "bool"
  f _ UnitType = "unit"
  f p (FunType t1 t2) = let s = f 1 t1 ++ " -> " ++ f 0 t2
                        in if p > 0 then "(" ++ s ++ ")" else s 
  f _ (TupleType ts) = "(" ++ intercalate ", " (map (f 0) ts) ++ ")" 
  f p (DataType ts n) = let s = intercalate " " $ show n : map (f 2) ts
                        in if p > 1 then "(" ++ s ++ ")" else s

showTypeVar :: TypeVar -> String
showTypeVar i = '\'' : map (chr . (97 +)) (digits 26 i)
 where
  digits base = reverse . go
   where
    go 0 = []
    go k = (k `mod` base) : go (k `div` base)

returnType :: Type -> Int -> Type
returnType t 0 = t
returnType (FunType _ t) i = returnType t (i - 1)
returnType t _ = t

prefixOfType :: Type -> String
prefixOfType (FunType _ _) = "f"
prefixOfType (TupleType _) = "t"
prefixOfType t = take 1 $ show t
