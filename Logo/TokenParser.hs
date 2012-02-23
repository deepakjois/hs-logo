module Logo.TokenParser where

import Logo.Types

import Control.Applicative ((<|>), (<$>), many)

import Text.ParserCombinators.Parsec (
  char, letter, digit, alphaNum, string, space,
  parse, many1, skipMany, notFollowedBy, noneOf, try, (<?>), eof,
  ParseError, Parser)

import Text.ParserCombinators.Parsec.Number (natFloat, sign)

tokenize :: String -> String -> Either ParseError [LogoToken]
tokenize progName = parse logo progName

logo :: Parser [LogoToken]
logo = do
  skipMany space
  expressions <- many1 logoExpr
  skipMany space
  eof
  return $ concat expressions

logoExpr :: Parser [LogoToken]
logoExpr =  try list
        <|> try binaryExpr
        <|> try parenExpr
        <|> try word
        <?> "Logo Expression"

word :: Parser [LogoToken]
word =  try identifier
    <|> try stringLiteral
    <|> try varLiteral
    <|> try numLiteral
    <?> "Logo terminal"

identifier :: Parser [LogoToken]
identifier = do
  skipMany space
  s <- letter
  i <- many alphaNum
  return . return $  (Identifier (s:i))

-- FIXME support escaping
stringLiteral :: Parser [LogoToken]
stringLiteral = do
  skipMany space
  char '"'
  s <- many1 $ noneOf "\t\n []()\""
  return . return $ StrLiteral s

varLiteral :: Parser [LogoToken]
varLiteral = do
  skipMany space
  char ':'
  s <- letter
  v <- many alphaNum
  return . return $ VarLiteral (s:v)

numLiteral :: Parser [LogoToken]
numLiteral = do
  skipMany space
  s <- sign
  n <- natFloat
  return . return . NumLiteral . s $ case n of
    Left i  -> fromInteger i
    Right f -> f

operExpr :: Parser [LogoToken]
operExpr =  try parenExpr
        <|> try word

binaryExpr :: Parser [LogoToken]
binaryExpr = do
  lhs <- operExpr
  op  <- operLiteral
  rhs <- try binaryExpr <|> operExpr
  return . concat $ [lhs, op, rhs]

operLiteral :: Parser [LogoToken]
operLiteral = do
  s <- many space
  (return . OperLiteral) <$>
    (  string "+"
   <|> if (length s) == 0 then (string "-") else ((string "-") >> notFollowedBy digit >> (return "-"))
   <|> string "*"
   <|> string "/"
   <|> string "%"
   <|> string "^"
   <|> try (string ">=")
   <|> try (string "<=")
   <|> try (string "<>")
   <|> string "="
   <|> string "<"
   <|> string ">" )

list :: Parser [LogoToken]
list = do
  skipMany space
  char '['
  expr <- many logoExpr
  skipMany space
  char ']'
  return . return $ LogoList (concat expr)

parenExpr :: Parser [LogoToken]
parenExpr = do
  skipMany space
  char '('
  skipMany space
  expr <- many logoExpr
  skipMany space
  char ')'
  return . return $ LogoExpr (concat expr)