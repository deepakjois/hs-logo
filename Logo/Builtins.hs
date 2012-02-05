module Logo.Builtins where

import Logo.Types
import Logo.Turtle
import Text.Parsec.Prim
import Control.Applicative ((<$>))

import qualified Data.Map as M

fd :: [LogoToken] -> LogoEvaluator LogoToken
fd ((NumLiteral d):[]) = do
  updateTurtleState (forward d)
  return $ StrLiteral ""

updateTurtleState :: (Turtle -> Turtle) -> LogoEvaluator ()
updateTurtleState f = do
  s <- getState
  let t = turtle s
  putState $ s { turtle = f t }

builtins = M.fromList
  [("fd", LogoFunctionDef 1 fd)]