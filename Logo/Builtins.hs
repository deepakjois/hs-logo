module Logo.Builtins where

import Logo.Types
import Logo.Turtle

import qualified Data.Map as M

fd :: [LogoToken] -> LogoContext -> LogoContext
fd ((NumLiteral d):[]) (LogoContext t f) = LogoContext (forward d t) f

builtins = M.fromList
  [("fd", LogoFunctionDef 1 (B fd))]