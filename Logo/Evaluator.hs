{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Logo.Evaluator where

import Logo.Types

import Text.Parsec.Prim

-- ----------------------------------------------------------------------

--  Expression Evaluation

-- ----------------------------------------------------------------------

--  Expression               := RelationalExpression
--  RelationalExpression     := AdditiveExpression [ ( '=' | '<' | '>' | '<=' | '>=' | '<>' ) AdditiveExpression ... ]
--  AdditiveExpression       := MultiplicativeExpression [ ( '+' | '-' ) MultiplicativeExpression ... ]
--  MultiplicativeExpression := PowerExpression [ ( '*' | '/' | '%' ) PowerExpression ... ]
--  PowerExpression          := UnaryExpression [ '^' UnaryExpression ]
--  UnaryExpression          := ( '-' ) UnaryExpression
--                            | FinalExpression
--  FinalExpression          := string-literal
--                            | number-literal
--                            | list
--                            | variable-reference
--                            | procedure-call
--                            | '(' Expression ')'


evaluate :: [LogoToken] -> IO ()
evaluate tokens = do
  case runParser expression () "(stream)" tokens of
    Right _ -> putStrLn "Error!"
    Left _  -> putStrLn "Done.."

expression :: Parsec [LogoToken] st ()
expression = undefined
