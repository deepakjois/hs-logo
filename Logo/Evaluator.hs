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

logoToken :: LogoToken -> Parsec [LogoToken] st LogoToken
logoToken x = satisfy (==x)

anyLogoToken :: Parsec [LogoToken] st LogoToken
anyLogoToken = satisfy (const True)

expression :: Parsec [LogoToken] st [String]
expression = many1 relationalExpression

relationalExpression :: Parsec [LogoToken] st String
relationalExpression = parseWithOperators ["<", ">", "=", "<=", ">=", "<>"] additiveExpression

additiveExpression :: Parsec [LogoToken] st String
additiveExpression = parseWithOperators ["+", "-"] multiplicativeExpression

multiplicativeExpression :: Parsec [LogoToken] st String
multiplicativeExpression = parseWithOperators ["*", "/", "%"] finalExpression

finalExpression = do
  token <- anyLogoToken
  case token of
    NumLiteral n -> return $ show n
    StrLiteral s -> return s
    VarLiteral v -> return $ "var" ++ v
    Identifier s -> return  "<function dispatch>"
    _            -> return "TBD"

parseWithOperators operators parser = do
  lhs <- parser
  option lhs $ do
    op <- choice $ map (logoToken . OperLiteral) operators
    rhs <- parser
    return $ eval op lhs rhs
   where eval o l r = "(" ++ (show l) ++ ")" ++ (show o) ++ "(" ++ (show r) ++ ")"
