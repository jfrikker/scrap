module JIR.Type (

) where

import qualified LLIR

data Type = FunctionType Type Type |
  I64Type

hash :: Type -> String
hash I64Type = "i64"

toLLIRType :: Type -> LLIR.Type
toLLIRType I64Type = LLIR.I64Type

returnType :: Type -> Type
returnType (FunctionType _ r) = r
