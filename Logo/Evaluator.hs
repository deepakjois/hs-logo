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

data LogoTokens = LogoTokens [LogoToken]

instance (Monad m) => Stream LogoTokens m LogoToken where
    uncons (LogoTokens [])      = return Nothing
    uncons (LogoTokens (x:xs))  = return $ Just (x, LogoTokens xs)

evaluate :: [LogoToken] -> IO ()
evaluate tokens = do
  case runParser expression () "(stream)" (LogoTokens tokens) of
    Right _ -> putStrLn "Error!"
    Left _  -> putStrLn "Done.."

expression :: Parsec LogoTokens st ()
expression = undefined


