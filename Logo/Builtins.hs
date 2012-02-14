module Logo.Builtins where

import Logo.Types
import Logo.Evaluator

import qualified Data.Map as M

import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)

import Text.Parsec.Prim (getState, putState, modifyState, many)
import Text.Parsec.Combinator (manyTill)

import Diagrams.TwoD.Path.Turtle

fd, bk, rt, lt, repeat_, to, ifelse :: [LogoToken] -> LogoEvaluator LogoToken

fd (NumLiteral d : []) = do
  updateTurtleState (forward d)
  return $ StrLiteral ""

fd _ = error "Invalid arguments to fd"

bk (NumLiteral d : []) = do
  updateTurtleState (backward d)
  return $ StrLiteral ""

bk _ = error "Invalid arguments to fd"

rt (NumLiteral a : []) = do
  updateTurtleState (right a)
  return $ StrLiteral ""

rt _ = error "Invalid arguments to rt"

lt (NumLiteral a : []) = do
  updateTurtleState (left a)
  return $ StrLiteral ""

lt _ = error "Invalid arguments to lt"

repeat_ (NumLiteral n : (t@(LogoList _) : []))
  | n == 0    = return $ StrLiteral ""
  | otherwise = do evaluateList t
                   repeat_ [NumLiteral (n - 1 :: Double), t]

repeat_ _ = error "Invalid arguments for repeat"

ifelse [StrLiteral val, ifList, elseList]
  | val == "TRUE"  = evaluateList ifList
  | val == "FALSE" = evaluateList elseList

ifelse _ = error "Invalid arguments for if"

to [] = do
  (Identifier name) <- anyLogoToken
  args <- map fromVar <$> many (satisfy isVarLiteral)
  tokens <- manyTill anyLogoToken (logoToken $ Identifier "end")
  modifyState (addFunction name $ LogoFunctionDef (length args) (createLogoFunction args tokens))
  return $ StrLiteral ""
 where
  isVarLiteral (VarLiteral _) = True
  isVarLiteral _              = False

  fromVar (VarLiteral s)      = s
  fromVar _                   = undefined

  addFunction name fn (LogoContext f v) = LogoContext (M.insert name fn f) v

to _ = undefined

createLogoFunction ::  [String] -> [LogoToken] -> LogoFunction
createLogoFunction vars_ tokens_ = \args -> do
  st <- getState
  modifyState (addArgsToContext $ zip vars_ args)
  tokens <- evaluateTokens tokens_
  final <- getState
  putState $  final { vars = vars st }
  return tokens
 where
  addArgsToContext a (LogoContext f v) = LogoContext f (M.fromList a `M.union`  v)

updateTurtleState :: Turtle a  ->  LogoEvaluator a
updateTurtleState = lift

builtins :: M.Map String LogoFunctionDef
builtins = M.fromList
  [ ("fd",     LogoFunctionDef 1 fd)
  , ("bk",     LogoFunctionDef 1 bk)
  , ("rt",     LogoFunctionDef 1 rt)
  , ("lt",     LogoFunctionDef 1 lt)
  , ("repeat", LogoFunctionDef 2 repeat_)
  , ("to",     LogoFunctionDef 0 to)
  , ("ifelse", LogoFunctionDef 3 ifelse)
  ]