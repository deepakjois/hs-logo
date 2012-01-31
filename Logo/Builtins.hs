module Logo.Builtins where

import Logo.Types
import Logo.Turtle

import qualified Data.Map as M

fd :: [LogoToken] -> LogoEvaluator LogoToken
fd ((NumLiteral d):[]) = undefined  -- (LogoContext t f) = unLogoContext (forward d t) f

builtins = M.fromList
  [("fd", LogoFunctionDef 1 fd)]