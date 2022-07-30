module AST.ASTRoot (
  Module(..)
) where

import AST.Expression (ASTExpression)
import AST.Types (ASTType)
import Data.Map (Map)

data Global = Constant ASTType ASTExpression

data Module = Module (Map String Global)
