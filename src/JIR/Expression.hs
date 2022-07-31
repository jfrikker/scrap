module JIR.Expression (
  Expression(..)
) where

data Expression = Binary Prim.BinaryOperation Expression Expression |
  Call Expression Expression |
  I64Literal Word64 |
  Reference String

dataType :: Expression -> Map String Definition -> Type
dataType (Binary _ a1 _) scope = dataType a1 scope
dataType (Call func arg) scope = unquantifyCall (dataType func scope) $ dataType arg scope
dataType (I64Literal _) _ = I64Type
dataType (Reference name) scope = snd $ Maybe.fromJust $ Map.lookup name scope
