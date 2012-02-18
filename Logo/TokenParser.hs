module Logo.TokenParser where

import Logo.Types

import Control.Applicative ((<|>), (<$>), many)

import Text.ParserCombinators.Parsec (
  char, letter, alphaNum, string, space,
  parse, many1, skipMany, skipMany1, sepEndBy, noneOf, try,
  ParseError, Parser)

import Text.ParserCombinators.Parsec.Number (natFloat, sign)

tokenize :: String -> Either ParseError [LogoToken]
tokenize = parse logo "(unknown)"

logo :: Parser [LogoToken]
logo = do
  skipMany space
  sepEndBy atom (skipMany1 space)

atom :: Parser LogoToken
atom =  identifier <|> stringLiteral <|> varLiteral <|> list <|> try numLiteral <|> try operLiteral <|> list <|> expression

identifier :: Parser LogoToken
identifier = do
  s <- letter
  i <- many alphaNum
  return $ Identifier (s:i)

stringLiteral :: Parser LogoToken
stringLiteral = do
  char '"'
  s <- many1 $ noneOf "\t\n []"
  return $ StrLiteral s

varLiteral :: Parser LogoToken
varLiteral = do
  char ':'
  s <- letter
  v <- many alphaNum
  return $ VarLiteral (s:v)

numLiteral :: Parser LogoToken
numLiteral = do
  s <- sign
  n <- natFloat
  return . NumLiteral . s $ case n of
             Left i  -> fromInteger i
             Right f -> f

operLiteral :: Parser LogoToken
operLiteral =  OperLiteral <$>
       (  string "+"
      <|> string "-"
      <|> string "*"
      <|> string "/"
      <|> string "%"
      <|> string "^"
      <|> try (string ">=")
      <|> try (string "<=")
      <|> try (string "<>")
      <|> string "="
      <|> string "<"
      <|> string ">"
       )

list :: Parser LogoToken
list = do
  char '['
  atoms <- logo
  char ']'
  return $  LogoList atoms

expression :: Parser LogoToken
expression = do
  char '('
  atoms <- logo
  char ')'
  return $ LogoExpr atoms