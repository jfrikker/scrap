module Joe.Parser (
  moduleParser
) where

import Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Joe.LLIR as LLIR
import Text.Parsec((<|>), many, many1, option, sepBy1)
import Text.Parsec.Char (alphaNum, char, digit, lower, oneOf, spaces, string, upper)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (makeTokenParser, GenLanguageDef(..), GenTokenParser)

moduleParser :: Parser LLIR.Globals
moduleParser = do
  spaces
  globs <- many functionDef
  return $ Map.fromList globs

identifier :: Parser String
identifier = do
  first <- lower <|> char '_'
  rest <- many $ alphaNum <|> char '_'
  spaces
  return $ first : rest

typeIdentifier :: Parser String
typeIdentifier = do
  first <- upper
  rest <- many $ alphaNum <|> char '_'
  spaces
  return $ first : rest

dataType :: Parser LLIR.Type
dataType = do
  t <- typeIdentifier
  case t of
    "I64" -> return LLIR.I64Type
    otherwise -> fail $ "Unknown type " ++ t

typeQualifier :: Parser LLIR.Type
typeQualifier = do
  char(':')
  spaces
  dataType

functionDef :: Parser (String, LLIR.Global)
functionDef = do
  name <- identifier
  args <- option [] $ do
    char('(')
    spaces
    a <- (flip sepBy1) (char ',' >> spaces) $ do
      argName <- identifier
      argType <- typeQualifier
      return (argName, argType)
    char(')')
    spaces
    return a
  t <- typeQualifier
  char ('=')
  spaces
  body <- expression
  char(';')
  spaces
  return $ (name, LLIR.Global args body)

expression :: Parser LLIR.Expression
expression = atom

atom :: Parser LLIR.Expression
atom = i64Literal

i64Literal :: Parser LLIR.Expression
i64Literal = do
  num <- many1 digit
  string "i64"
  spaces
  return $ LLIR.I64Literal $ read num