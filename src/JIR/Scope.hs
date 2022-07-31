module JIR.Scope (

) where

import JIR.Expression (Expression)

data Definition = ExpressionDefinition Expression |
  FunctionDefinition Type String Expression

newtype Scope = Scope (Map String Definition)
