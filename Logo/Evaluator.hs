module Logo.Evaluator where

import Logo.Types

import Text.Parsec.Prim
import Text.Parsec.Combinator

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
    Right s -> putStrLn $ "Done " ++ (show s)
    Left e  -> putStrLn $ "Error.." ++ (show e)

satisfy ::  (LogoToken -> Bool) -> Parsec [LogoToken] st LogoToken
satisfy f =
  tokenPrim (\c -> show [c])
            (\pos c _cs ->  pos)
            (\c -> if f c then Just c else Nothing)

logotoken x = satisfy (==x)

expression :: Parsec [LogoToken] st [String]
expression = many1 relationalExpression

relationalExpression :: Parsec [LogoToken] st String
relationalExpression = do
  lhs <- additiveExpression
  parseRhs(lhs) <|> return lhs
 where
   parseRhs lhs = do
     opL <- choice $ map (logotoken . OperLiteral) ["<", ">", "=", "<=", ">=", "<>"]
     let OperLiteral op  = opL
     rhs <- additiveExpression
     return $ eval op lhs rhs
    where
      eval op lhs rhs = lhs ++ op ++ rhs

additiveExpression :: Parsec [LogoToken] st String
additiveExpression = do
  lhs <- multiplicativeExpression
  parseRhs(lhs) <|> return lhs
 where
   parseRhs lhs = do
     opL <- choice $ map (logotoken . OperLiteral) ["+", "-"]
     let OperLiteral op = opL
     rhs <- multiplicativeExpression
     return $ eval op lhs rhs
    where
      eval op lhs rhs = lhs ++ op ++ rhs


multiplicativeExpression = do
  lhs <- finalExpression
  parseRhs(lhs) <|> return lhs
 where
   parseRhs lhs = do
     opL <- choice $ map (logotoken . OperLiteral) ["*", "/", "%"]
     let OperLiteral op = opL
     rhs <- finalExpression
     return $ eval op lhs rhs
    where
      eval op lhs rhs = lhs ++ op ++ rhs

finalExpression = do
  token <- satisfy (const True)
  case token of
    NumLiteral n -> return $ show n
    StrLiteral s -> return s
    VarLiteral v -> return $ "var" ++ v
    Identifier s -> return  "<function dispatch>"
    _            -> return "TBD"
