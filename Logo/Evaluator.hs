module Logo.Evaluator where

import Logo.Types
import Logo.Builtins (builtins)

import qualified Data.Map as M


import Control.Monad (replicateM)

import Text.Parsec.Prim
import Text.Parsec.Combinator

import Diagrams.Prelude


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


evaluate :: [LogoToken] -> ([String], LogoContext)
evaluate tokens = do
  let t = Turtle True 0 (Path [(P (0,0), Trail [] False)])
      ctx = LogoContext t builtins
  case runParser expression ctx "(stream)" tokens of
    Right s -> s
    Left e  -> error (show e)

satisfy ::  (LogoToken -> Bool) -> LogoEvaluator LogoToken
satisfy f =
  tokenPrim (\c -> show [c])
            (\pos c _cs ->  pos)
            (\c -> if f c then Just c else Nothing)

logoToken :: LogoToken -> LogoEvaluator LogoToken
logoToken x = satisfy (==x)

anyLogoToken :: LogoEvaluator LogoToken
anyLogoToken = satisfy (const True)

expression :: LogoEvaluator ([String], LogoContext)
expression = do
  tokens <- many1 relationalExpression
  state  <- getState
  return (tokens, state)

relationalExpression :: LogoEvaluator String
relationalExpression = parseWithOperators ["<", ">", "=", "<=", ">=", "<>"] additiveExpression

additiveExpression :: LogoEvaluator String
additiveExpression = parseWithOperators ["+", "-"] multiplicativeExpression

multiplicativeExpression :: LogoEvaluator String
multiplicativeExpression = parseWithOperators ["*", "/", "%"] finalExpression

finalExpression = do
  token <- anyLogoToken
  case token of
    NumLiteral n -> return $ show n
    StrLiteral s -> return s
    VarLiteral v -> return $ "var" ++ v
    Identifier s -> dispatchFn s
    _            -> return "TBD"

parseWithOperators :: [String] -> LogoEvaluator String  -> LogoEvaluator String
parseWithOperators operators parser = do
  lhs <- parser
  option lhs $ do
    op <- choice $ map (logoToken . OperLiteral) operators
    rhs <- parser
    return $ eval op lhs rhs
   where eval o l r = "(" ++ (show l) ++ ")" ++ (show o) ++ "(" ++ (show r) ++ ")"

dispatchFn :: String -> LogoEvaluator String
dispatchFn fn = do
  -- get function definition
  ctx <- getState
  let fns = functions ctx
      f = case M.lookup fn fns of
        Just x -> x
        _      -> error ("Function undefined: " ++ fn)
  -- find arity
  let (LogoFunctionDef a func) =  f
  -- get number of tokens
  -- FIXME evaludate the token before getting a list of expressions
  arguments <- replicateM a anyLogoToken
  -- call function and update context
  func arguments
