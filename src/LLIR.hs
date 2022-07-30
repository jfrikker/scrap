module LLIR (
  BinaryOperation(..),
  dataType,
  Expression(..),
  Function(..),
  Type(..),
) where

import Data.Word (Word8, Word16, Word32, Word64)

data BinaryOperation = Add |
  Subtract |
  Multiply |
  Divide

data Expression = Binary BinaryOperation Expression Expression |
  I64Literal Word64 |
  StaticFunctionCall String [Expression] Type |
  Argument Word16 Type

data Type = I64Type

data Function = Function [Type] Expression

dataType :: Expression -> Type
dataType (Argument _ t) = t
dataType (Binary _ a1 _) = dataType a1
dataType (I64Literal _) = I64Type
dataType (StaticFunctionCall _ _ t) = t
