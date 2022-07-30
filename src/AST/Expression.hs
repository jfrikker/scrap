module AST.Expression (
  ASTExpression(..)
) where

import AST.Token (Token)

data ASTExpression = Identifier Token
