module AST.Types (
  ASTType(..)
) where

import AST.Token (Token)

data ASTType = Simple Token |
  Composite ASTType ASTType |
  Function ASTType ASTType
