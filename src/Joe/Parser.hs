module Joe.Parser (
  moduleParser
) where

import Data.Functor.Identity (Identity)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Joe.LLIR as LLIR
import qualified Joe.Prim as Prim
import Text.Parsec((<|>), choice, many, many1, option, sepBy1)
import Text.Parsec.Char (alphaNum, char, digit, lower, oneOf, spaces, string, upper)
import Text.Parsec.Expr (Assoc(AssocLeft), buildExpressionParser, Operator(Infix), OperatorTable)
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
  return $ (name, LLIR.Global args t body)

exprTable :: OperatorTable Text () Identity LLIR.Expression
exprTable = [
  [binary '+' Prim.Add AssocLeft]
  ]

binary :: Char -> Prim.BinaryOperation -> Assoc -> Operator Text () Identity LLIR.Expression
binary c op = Infix p
  where p = do
          char c
          spaces
          return $ LLIR.Binary op

expression :: Parser LLIR.Expression
expression = buildExpressionParser exprTable call

call :: Parser LLIR.Expression
call = do
  f <- atom
  calls <- many $ do
    char '('
    spaces
    args <- sepBy1 expression (char ',' >> spaces)
    char ')'
    spaces
    return args
  return $ List.foldl LLIR.Call f calls

atom :: Parser LLIR.Expression
atom = choice [
  scopeRef,
  i64Literal
  ]

i64Literal :: Parser LLIR.Expression
i64Literal = do
  num <- many1 digit
  string "i64"
  spaces
  return $ LLIR.I64Literal $ read num

scopeRef :: Parser LLIR.Expression
scopeRef = identifier >>= return . LLIR.Reference
