module Logo.Types where

import Data.Map (Map)
import Text.Parsec.Prim (Parsec)
import Diagrams.Prelude

data LogoToken = Identifier String -- ^ Identifier
               | StrLiteral String -- ^ String Literal, e.g @"word@
               | VarLiteral String -- ^ Variable, e.g @:size@
               | NumLiteral Double -- ^ Number
               | OperLiteral String  -- ^ Operator
               | List [LogoToken] -- ^ Input definition/variable reference
               deriving (Show, Eq)


type LogoEvaluator a = Parsec [LogoToken] LogoContext a
type LogoFunction = [LogoToken] -> LogoEvaluator LogoToken

data LogoFunctionDef = LogoFunctionDef
  { arity :: Int -- ^ Number of arguments
  , runFn :: LogoFunction -- ^ Consumes an argument
  }

data Turtle = Turtle
  Bool -- ^ Pen up position
  Deg -- ^ Angle of Turtle
  (Path R2) -- ^ Segments drawn so far

data LogoContext = LogoContext
  { turtle :: Turtle -- ^ Turtle graphics context
  , functions :: Map String LogoFunctionDef -- ^ Functions that can be called, mapped by the identifier
  }

instance Show LogoContext where
  show x = "<context>"
