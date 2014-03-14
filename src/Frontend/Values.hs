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

typeOfValue :: Value -> Type
typeOfValue (IntValue _) = IntType
typeOfValue (FloatValue _) = FloatType
typeOfValue (BoolValue _) = BoolType
typeOfValue UnitValue = UnitType
