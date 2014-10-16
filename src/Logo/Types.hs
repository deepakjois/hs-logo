module Logo.Types where

import Data.Map (Map)
import Text.Parsec.Prim (ParsecT)
import Diagrams.TwoD.Path.Turtle (TurtleT)

data LogoToken = Identifier String    -- Identifier
               | StrLiteral String    -- String Literal, e.g @"word@
               | VarLiteral String    -- Variable, e.g @:size@
               | NumLiteral Double    -- Number
               | OperLiteral String   -- Operator
               | LogoList [LogoToken] -- Input definition/variable reference
               | LogoExpr [LogoToken] -- Expression inside parentheses
               deriving (Eq)

instance Show LogoToken where
  show (Identifier s)  = "@" ++ s ++ "@"
  show (StrLiteral s)  = s
  show (VarLiteral s)  = ":" ++ s
  show (NumLiteral s)  = show s
  show (OperLiteral s) = s
  show (LogoList l)    = show l
  show (LogoExpr e)    = "(" ++ show e ++ ")"

type TurtleIO = TurtleT IO

type LogoEvaluator  = ParsecT [LogoToken] LogoContext TurtleIO

type LogoFunction = [LogoToken] -> LogoEvaluator LogoToken

type LogoSymbolTable = Map String LogoToken

data LogoFunctionDef = LogoFunctionDef
  { arity :: Int          -- Number of arguments
  , runFn :: LogoFunction -- Consumes an argument
  }

data LogoContext = LogoContext
  { functions :: Map String LogoFunctionDef -- Functions that can be called, mapped by the identifier
  , locals  :: LogoSymbolTable -- Vars in local context
  , globals :: LogoSymbolTable -- Vars in global context
  }



