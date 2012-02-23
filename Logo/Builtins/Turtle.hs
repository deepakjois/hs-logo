module Logo.Builtins.Turtle where

import Control.Monad.Trans (lift)
import Diagrams.TwoD.Path.Turtle
import Diagrams.TwoD.Types (p2)

import Logo.Types

updateTurtle :: TurtleIO a  ->  LogoEvaluator a
updateTurtle = lift

fd, bk, rt, lt, home, setxy, seth, pu, pd :: [LogoToken] -> LogoEvaluator LogoToken

fd (NumLiteral d : []) = do
  updateTurtle (forward d)
  return $ StrLiteral ""

fd args = error $ "Invalid arguments to fd" ++ show args

bk (NumLiteral d : []) = do
  updateTurtle (backward d)
  return $ StrLiteral ""

bk _ = error "Invalid arguments to fd"

rt (NumLiteral a : []) = do
  updateTurtle (right a)
  return $ StrLiteral ""

rt _ = error "Invalid arguments to rt"

lt (NumLiteral a : []) = do
  updateTurtle (left a)
  return $ StrLiteral ""

lt _ = error "Invalid arguments to lt"

home [] = do
  updateTurtle (setPos (p2 (0,0)))
  return $ StrLiteral ""

home _ = error "Invalid arguments to home"

setxy [NumLiteral x, NumLiteral y] = do
  updateTurtle (setPos (p2 (x,y)))
  return $ StrLiteral ""

setxy _ = error "Invalid arguments to setxy"

seth [NumLiteral n] = do
  updateTurtle (setHeading n)
  return $ StrLiteral ""

seth _ = error "Invalid arguments to seth"

pu [] = do
  updateTurtle penUp
  return $ StrLiteral ""

pu _ = error "Invalid arguments to pu"

pd [] = do
  updateTurtle penDown
  return $ StrLiteral ""

pd _ = error "Invalid arguments to pd"
