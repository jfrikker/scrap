module Joe.Parser (
  moduleParser
) where

import Data.Function ((&))
import Data.Functor.Identity (Identity)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Joe.LLIR as LLIR
import qualified Joe.Prim as Prim
import Text.Parsec((<|>), choice, endBy, many, many1, option, sepBy, sepBy1, try)
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

qualifiedIdentifier :: Parser String
qualifiedIdentifier = do
  qs <- endBy qualifier $ char '.'
  i <- identifier
  return $ List.intercalate "." $ qs ++ [i]

qualifier :: Parser String
qualifier = typeIdentifier

typeIdentifier :: Parser String
typeIdentifier = do
  first <- upper
  rest <- many $ alphaNum <|> char '_'
  spaces
  return $ first : rest

dataType :: Parser LLIR.Type
dataType = functionType <|> nonFunctionType

functionType :: Parser LLIR.Type
functionType = do
  char '('
  spaces
  args <- sepBy1 dataType $ char ',' >> spaces
  char ')'
  spaces
  string "->"
  spaces
  retType <- nonFunctionType
  return $ LLIR.FunctionType args retType

nonFunctionType :: Parser LLIR.Type
nonFunctionType = do
  t <- typeIdentifier
  case t of
    "I64" -> return LLIR.I64Type
    otherwise -> fail $ "Unknown type " ++ t

typeQualifier :: Parser LLIR.Type
typeQualifier = do
  char(':')
  spaces
  dataType

constantDef :: Parser (String, LLIR.Global)
constantDef = do
  name <- identifier
  char(':')
  spaces
  t <- nonFunctionType
  spaces
  char ('=')
  spaces
  body <- assignmentBody
  return $ (name, LLIR.Global [] t body)

functionDef :: Parser (String, LLIR.Global)
functionDef = do
  qualifiedType <- option Nothing $ do
    r <- nonFunctionType
    char '.'
    spaces
    return $ Just r
  name <- qualifiedIdentifier
  (args, t) <- choice [try constantDecl, functionDecl]
  char ('=')
  spaces
  body <- assignmentBody
  case qualifiedType of
    Nothing -> return $ (name, LLIR.Global args t body)
    Just qt -> return $ ((LLIR.typeToQualifier qt) ++ name, LLIR.Global (("this", qt) : args) t body)
  where constantDecl = do
          char(':')
          spaces
          t <- nonFunctionType
          spaces
          return ([], t)
        functionDecl = do
          args <- argList
          string "->"
          spaces
          t <- nonFunctionType
          spaces
          return (args, t)

argList :: Parser [(String, LLIR.Type)]
argList = do
  char('(')
  spaces
  args <- (flip sepBy1) (char ',' >> spaces) $ do
    argName <- identifier
    argType <- typeQualifier
    return (argName, argType)
  char(')')
  spaces
  return args

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
expression = buildExpressionParser exprTable callOrMemberAccess

callOrMemberAccess :: Parser LLIR.Expression
callOrMemberAccess = do
  f <- atom
  elems <- many $ choice [call, memberAccess]
  return $ foldl (&) f elems

call :: Parser (LLIR.Expression -> LLIR.Expression)
call = do
  char '('
  spaces
  args <- sepBy1 expression (char ',' >> spaces)
  char ')'
  spaces
  return $ \f -> LLIR.Call f args

memberAccess :: Parser (LLIR.Expression -> LLIR.Expression)
memberAccess = do
  char '.'
  spaces
  member <- identifier
  return $ \o -> LLIR.MemberAccess o member

atom :: Parser LLIR.Expression
atom = choice [
  parens,
  block,
  lambda,
  scopeRef,
  i64Literal
  ]

lambda :: Parser LLIR.Expression
lambda = do
  char '\\'
  args <- argList
  char '='
  spaces
  body <- expression
  return $ LLIR.Lambda args body

i64Literal :: Parser LLIR.Expression
i64Literal = do
  num <- many1 digit
  string "i64"
  spaces
  return $ LLIR.I64Literal $ read num

scopeRef :: Parser LLIR.Expression
scopeRef = qualifiedIdentifier >>= return . LLIR.Reference

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
    args <- option [] argList
    char '='
    spaces
    body <- assignmentBody
    case args of
      [] -> return (name, body)
      otherwise -> return (name, LLIR.Lambda args body)
  e <- expression
  char '}'
  spaces
  return $ List.foldr (\(name, body) -> LLIR.Scope name body) e scopes
