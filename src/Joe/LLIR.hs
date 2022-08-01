module Joe.LLIR (
  dataType,
  Expression(..),
  Function(..),
  Type(..),
) where

import qualified Data.List as List
import Data.Word (Word8, Word16, Word32, Word64)
import qualified Joe.Prim as Prim

data Expression = Binary Prim.BinaryOperation Expression Expression |
  I64Literal Word64 |
  Call String [Expression] Type |
  Argument Word16 Type

data Type = I64Type

data Function = Function String [Type] Expression

dataType :: Expression -> Type
dataType (Argument _ t) = t
dataType (Binary _ a1 _) = dataType a1
dataType (I64Literal _) = I64Type
dataType (Call _ _ t) = t
