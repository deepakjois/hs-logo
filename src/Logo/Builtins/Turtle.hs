module Logo.Builtins.Turtle (turtleBuiltins) where

import Prelude hiding (tan)
import qualified Data.Map as M

import Control.Monad.Trans (lift)
import Diagrams.TwoD.Path.Turtle
import Diagrams.TwoD.Types (p2)
import Data.Colour (Colour)
import Data.Colour.Names

import Logo.Types

updateTurtle :: TurtleIO a  ->  LogoEvaluator a
updateTurtle = lift

fd, bk, rt, lt, home, setxy, seth, pu, pd, setpensize, setpencolor :: [LogoToken] -> LogoEvaluator LogoToken

turtleBuiltins :: M.Map String LogoFunctionDef
turtleBuiltins = M.fromList
  [ ("fd",          LogoFunctionDef 1 fd)
  , ("bk",          LogoFunctionDef 1 bk)
  , ("rt",          LogoFunctionDef 1 rt)
  , ("lt",          LogoFunctionDef 1 lt)
  , ("home",        LogoFunctionDef 0 home)
  , ("setxy",       LogoFunctionDef 2 setxy)
  , ("seth",        LogoFunctionDef 1 seth)
  , ("pu",          LogoFunctionDef 0 pu)
  , ("pd",          LogoFunctionDef 0 pd)
  , ("setpensize",  LogoFunctionDef 1 setpensize)
  , ("setpencolor", LogoFunctionDef 1 setpencolor)
  ]

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

setpensize [NumLiteral d] = do
  updateTurtle (setPenWidth d)
  return $ StrLiteral ""

setpensize _ = error  "Invalid arguments to setpensize"

setpencolor [NumLiteral d] = do
  updateTurtle (setPenColor (numToColor . round $ d))
  return $ StrLiteral ""
 where
  numToColor :: Int -> Colour Double
  numToColor 0  = black
  numToColor 1  = blue
  numToColor 2  = green
  numToColor 3  = cyan
  numToColor 4  = red
  numToColor 5  = magenta
  numToColor 6  = yellow
  numToColor 7  = white
  numToColor 8  = brown
  numToColor 9  = tan
  numToColor 10 = forestgreen
  numToColor 11 = aqua
  numToColor 12 = salmon
  numToColor 13 = purple
  numToColor 14 = orange
  numToColor 15 = grey

setpencolor _ = error "Invalid arguments to setpencolor"