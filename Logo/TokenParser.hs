module Logo.TokenParser where

import Data.Char
import Numeric
import Control.Applicative

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Logo.Types

tokenize :: String -> Either ParseError [LogoToken]
tokenize input = parse logo "(unknown)" input

logo :: Parser [LogoToken]
logo = do
  skipMany space
  sepEndBy1 atom (skipMany1 space)

atom :: Parser LogoToken
atom =  do
  a <- identifier <|> stringLiteral <|> varLiteral <|> list <|> numLiteral <|> operLiteral <|> list
  return a

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
  <|> string ">="
  <|> string "<="
  <|> string "<>"
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
  return $  List atoms
