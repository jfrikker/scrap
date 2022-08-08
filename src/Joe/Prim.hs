module Joe.Prim (
  BinaryOperation(..)
) where

data BinaryOperation = Add |
  Subtract |
  Multiply |
  Divide deriving (Eq, Show)
