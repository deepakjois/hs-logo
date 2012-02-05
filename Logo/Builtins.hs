module Logo.Builtins where

import Logo.Types
import Logo.Turtle
import Logo.Evaluator
import Text.Parsec.Prim
import Control.Applicative ((<$>))

import qualified Data.Map as M

fd, rt, repeat_ :: [LogoToken] -> LogoEvaluator LogoToken

fd ((NumLiteral d):[]) = do
  updateTurtleState (forward d)
  return $ StrLiteral ""

rt ((NumLiteral a):[]) = do
  updateTurtleState (right a)
  return $ StrLiteral ""

-- TODO add bk and lt

repeat_ ((NumLiteral n):t@(LogoList l):[])
  | n == 0    = return $ StrLiteral ""
  | otherwise = do evaluateList t
                   repeat_ ((NumLiteral (n-1::Double):t:[]))

updateTurtleState :: (Turtle -> Turtle) -> LogoEvaluator ()
updateTurtleState f = do
  s <- getState
  let t = turtle s
  putState $ s { turtle = f t }

builtins = M.fromList
  [ ("fd", LogoFunctionDef 1 fd)
  , ("rt", LogoFunctionDef 1 rt)
  , ("repeat", LogoFunctionDef 2 repeat_)
  ]