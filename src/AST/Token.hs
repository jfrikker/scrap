module AST.Token (
  Token(..)
) where

data Token = Token {
  text :: String,
  start :: Int,
  end :: Int
}
