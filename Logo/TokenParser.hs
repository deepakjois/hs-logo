module Logo.TokenParser where

import Logo.Types

import Control.Applicative ((<|>), (<$>), many)

import Text.ParserCombinators.Parsec (
  digit, char, letter, alphaNum, string, space,
  parse, many1, skipMany, skipMany1, sepEndBy1, noneOf, try,
  ParseError, Parser)

tokenize :: String -> Either ParseError [LogoToken]
tokenize = parse logo "(unknown)"

logo :: Parser [LogoToken]
logo = do
  skipMany space
  sepEndBy1 atom (skipMany1 space)

atom :: Parser LogoToken
atom =  identifier <|> stringLiteral <|> varLiteral <|> list <|> numLiteral <|> operLiteral <|> list

identifier :: Parser LogoToken
identifier = do
  s <- letter
  i <- many alphaNum
  return $ Identifier (s:i)

stringLiteral :: Parser LogoToken
stringLiteral = do
  char '"'
  s <- many1 $ noneOf "\t\n "
  return $ StrLiteral s

varLiteral :: Parser LogoToken
varLiteral = do
  char ':'
  s <- letter
  v <- many alphaNum
  return $ VarLiteral (s:v)

-- FIXME this is just lame! Use a *real* number parser. Also fix the unary minus problem
numLiteral :: Parser LogoToken
numLiteral = do
  s <- many1 (digit <|> char '.')
  return $ NumLiteral $  read s

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
  <|> string "("
  <|> string ")"
   )

list :: Parser LogoToken
list = do
  char '['
  atoms <- logo
  char ']'
  return $  LogoList atoms
