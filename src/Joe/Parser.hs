module Joe.Parser (
  moduleParser
) where

import Data.Functor.Identity (Identity)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Joe.LLIR as LLIR
import qualified Joe.Prim as Prim
import Text.Parsec((<|>), choice, endBy, many, many1, option, sepBy1, try)
import Text.Parsec.Char (alphaNum, char, digit, lower, oneOf, spaces, string, upper)
import Text.Parsec.Expr (Assoc(AssocLeft), buildExpressionParser, Operator(Infix), OperatorTable)
import Text.Parsec.Text (Parser)
import Text.Parsec.Token (makeTokenParser, GenLanguageDef(..), GenTokenParser)

import Debug.Trace (traceShowId)

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
dataType = functionType <|> do
  t <- typeIdentifier
  case t of
    "I64" -> return LLIR.I64Type
    otherwise -> fail $ "Unknown type " ++ t

functionType :: Parser LLIR.Type
functionType = do
  char '('
  spaces
  args <- sepBy1 dataType $ char ',' >> spaces
  char ')'
  spaces
  retType <- typeQualifier
  return $ LLIR.FunctionType args retType

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
  body <- assignmentBody
  return $ (name, LLIR.Global args t body)

assignmentBody :: Parser LLIR.Expression
assignmentBody = block <|> (do
    e <- expression
    char(';')
    spaces
    return e)

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
  parens,
  block,
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

parens :: Parser LLIR.Expression
parens = do
  char '('
  spaces
  e <- expression
  char ')'
  spaces
  return e

block :: Parser LLIR.Expression
block = do
  char '{'
  spaces
  scopes <- many $ try $ do
    name <- identifier
    char '='
    spaces
    body <- assignmentBody
    return (name, body)
  e <- expression
  char '}'
  spaces
  return $ List.foldr (\(name, body) -> LLIR.Scope name body) e scopes
