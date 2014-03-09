module Frontend.Values where

import Frontend.Types

import Internal

data Value = IntValue Int
           | FloatValue Float
           | BoolValue Bool
           | UnitValue
           deriving Eq

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
