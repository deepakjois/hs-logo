module Logo.Builtins where

import Logo.Types
import Logo.Turtle
import Logo.Evaluator
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Control.Applicative ((<$>))

import qualified Data.Map as M

fd, rt, lt, repeat_, to, ifelse :: [LogoToken] -> LogoEvaluator LogoToken

fd ((NumLiteral d):[]) = do
  updateTurtleState (forward d)
  return $ StrLiteral ""

rt ((NumLiteral a):[]) = do
  updateTurtleState (right a)
  return $ StrLiteral ""

lt ((NumLiteral a):[]) = do
  updateTurtleState (left a)
  return $ StrLiteral ""

-- TODO add bk and lt

updateTurtleState :: (Turtle -> Turtle) -> LogoEvaluator ()
updateTurtleState f = do
  s <- getState
  let t = turtle s
  putState $ s { turtle = f t }

repeat_ ((NumLiteral n):t@(LogoList l):[])
  | n == 0    = return $ StrLiteral ""
  | otherwise = do evaluateList t
                   repeat_ ((NumLiteral (n-1::Double):t:[]))


ifelse ((StrLiteral val):ifList:elseList:[])
  | val == "TRUE"  = evaluateList ifList
  | val == "FALSE" = evaluateList elseList

to [] = do
  (Identifier name) <- anyLogoToken
  vars <- map fromVar <$> many (satisfy isVarLiteral)
  tokens <- manyTill anyLogoToken (logoToken $ Identifier "end")
  modifyState (addFunction name $ LogoFunctionDef (length vars) (createLogoFunction vars tokens))
  return $ StrLiteral ""
 where
  isVarLiteral (VarLiteral _) = True
  isVarLiteral _              = False

  fromVar (VarLiteral s)      = s
  fromVar _                   = undefined

  addFunction name fn (LogoContext t f v) = LogoContext t (M.insert name fn f) v

createLogoFunction ::  [String] -> [LogoToken] -> LogoFunction
createLogoFunction vars_ tokens = \args -> do
  st <- getState
  modifyState (addArgsToContext $ zip vars_ args)
  tokens <- evaluateTokens tokens
  final <- getState
  putState $  final { vars = (vars st) }
  return tokens
 where
  addArgsToContext a (LogoContext t f v) = LogoContext t f (M.union (M.fromList a) v)

builtins = M.fromList
  [ ("fd", LogoFunctionDef 1 fd)
  , ("rt", LogoFunctionDef 1 rt)
  , ("lt", LogoFunctionDef 1 lt)
  , ("repeat", LogoFunctionDef 2 repeat_)
  , ("to", LogoFunctionDef 0 to)
  , ("ifelse", LogoFunctionDef 3 ifelse)
  ]