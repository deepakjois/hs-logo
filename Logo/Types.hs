module Logo.Types where

import Data.Map (Map)
import Text.Parsec.Prim (ParsecT)
import Diagrams.TwoD.Path.Turtle (Turtle)

data LogoToken = Identifier String -- ^ Identifier
               | StrLiteral String -- ^ String Literal, e.g @"word@
               | VarLiteral String -- ^ Variable, e.g @:size@
               | NumLiteral Double -- ^ Number
               | OperLiteral String  -- ^ Operator
               | LogoList [LogoToken] -- ^ Input definition/variable reference
               deriving (Show, Eq)


type LogoEvaluator  = ParsecT [LogoToken] LogoContext Turtle

type LogoFunction = [LogoToken] -> LogoEvaluator LogoToken

data LogoFunctionDef = LogoFunctionDef
  { arity :: Int -- ^ Number of arguments
  , runFn :: LogoFunction -- ^ Consumes an argument
  }

data LogoContext = LogoContext
  { functions :: Map String LogoFunctionDef -- ^ Functions that can be called, mapped by the identifier
  , vars :: Map String LogoToken -- ^ Symbol table mapping the name of a var to a value
  }

instance Show LogoContext where
  show _ = "<context>"
