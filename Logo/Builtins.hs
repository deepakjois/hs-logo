module Logo.Builtins where

import qualified Data.Map as M

import Logo.Types

import Logo.Builtins.Control
import Logo.Builtins.Arithmetic
import Logo.Builtins.Turtle
import Logo.Builtins.IO

builtins :: M.Map String LogoFunctionDef
builtins = M.fromList
  [ ("fd",       LogoFunctionDef 1 fd)
  , ("bk",       LogoFunctionDef 1 bk)
  , ("rt",       LogoFunctionDef 1 rt)
  , ("lt",       LogoFunctionDef 1 lt)
  , ("home",     LogoFunctionDef 0 home)
  , ("setxy",    LogoFunctionDef 2 setxy)
  , ("seth",     LogoFunctionDef 1 seth)
  , ("pu",       LogoFunctionDef 0 pu)
  , ("pd",       LogoFunctionDef 0 pd)
  , ("repeat",   LogoFunctionDef 2 repeat_)
  , ("repcount", LogoFunctionDef 0 repcount)
  , ("for",      LogoFunctionDef 2 for)
  , ("dotimes",  LogoFunctionDef 2 dotimes)
  , ("to",       LogoFunctionDef 0 to)
  , ("if",       LogoFunctionDef 2 if_)
  , ("ifelse",   LogoFunctionDef 3 ifelse)
  , ("sin",      LogoFunctionDef 1 sin_)
  , ("cos",      LogoFunctionDef 1 cos_)
  , ("tan",      LogoFunctionDef 1 tan_)
  , ("arctan",   LogoFunctionDef 1 arctan)
  , ("sqrt",     LogoFunctionDef 1 sqrt_)
  , ("pr",       LogoFunctionDef 1 pr)
  , ("random",   LogoFunctionDef 1 random)
  ]