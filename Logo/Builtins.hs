module Logo.Builtins (builtins) where

import qualified Data.Map as M

import Logo.Types

import Logo.Builtins.Control (controlBuiltins)
import Logo.Builtins.Arithmetic (arithmeticBuiltins)
import Logo.Builtins.Turtle (turtleBuiltins)
import Logo.Builtins.IO (ioBuiltins)

builtins :: M.Map String LogoFunctionDef
builtins = foldl1 M.union $
  [ controlBuiltins
  , arithmeticBuiltins
  , turtleBuiltins
  , ioBuiltins
  ]
