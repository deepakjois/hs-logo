module Logo.Evaluator where

import Logo.Types

import qualified Data.Map as M

import Control.Monad (replicateM)
import Control.Applicative ((<$>))

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

evaluateWithContext :: [LogoToken] -> LogoContext -> ([LogoToken], LogoContext)
evaluateWithContext tokens ctx = do
  case runParser expression ctx "(stream)" tokens of
    Right s -> s
    Left e  -> error (show e)

evaluateList :: LogoToken ->  LogoEvaluator LogoToken
evaluateList (LogoList l) = do
  (t,s) <- evaluateWithContext l <$> getState
  putState s
  return $ LogoList t

satisfy ::  (LogoToken -> Bool) -> LogoEvaluator LogoToken
satisfy f =
  tokenPrim (\c -> show [c])
            (\pos c _cs ->  pos)
            (\c -> if f c then Just c else Nothing)

logoToken :: LogoToken -> LogoEvaluator LogoToken
logoToken x = satisfy (==x)

anyLogoToken :: LogoEvaluator LogoToken
anyLogoToken = satisfy (const True)

expression :: LogoEvaluator ([LogoToken], LogoContext)
expression = do
  tokens <- many1 relationalExpression
  state  <- getState
  return (tokens, state)

relationalExpression :: LogoEvaluator LogoToken
relationalExpression = parseWithOperators ["<", ">", "=", "<=", ">=", "<>"] additiveExpression

additiveExpression :: LogoEvaluator LogoToken
additiveExpression = parseWithOperators ["+", "-"] multiplicativeExpression

multiplicativeExpression :: LogoEvaluator LogoToken
multiplicativeExpression = parseWithOperators ["*", "/", "%"] finalExpression

finalExpression :: LogoEvaluator LogoToken
finalExpression = do
  token <- anyLogoToken
  case token of
    Identifier s   -> dispatchFn s
    _              -> return token

parseWithOperators :: [String] -> LogoEvaluator LogoToken  -> LogoEvaluator LogoToken
parseWithOperators operators parser = do
  lhs <- parser
  option lhs $ do
    op <- choice $ map (logoToken . OperLiteral) operators
    rhs <- parser
    return $ eval op lhs rhs

eval :: LogoToken -> LogoToken -> LogoToken -> LogoToken

-- Arithmetic
eval (OperLiteral "+") (NumLiteral l) (NumLiteral r) = NumLiteral (l + r)
eval (OperLiteral "-") (NumLiteral l) (NumLiteral r) = NumLiteral (l - r)
eval (OperLiteral "*") (NumLiteral l) (NumLiteral r) = NumLiteral (l * r)
eval (OperLiteral "/") (NumLiteral l) (NumLiteral r) = NumLiteral (l / r)

-- Logical
eval (OperLiteral "<")  (NumLiteral l) (NumLiteral r) = StrLiteral (if l < r then "TRUE" else "FALSE")
eval (OperLiteral ">")  (NumLiteral l) (NumLiteral r) = StrLiteral (if l > r then "TRUE" else "FALSE")
eval (OperLiteral "=")  (NumLiteral l) (NumLiteral r) = StrLiteral (if l == r then "TRUE" else "FALSE")
eval (OperLiteral "<>") (NumLiteral l) (NumLiteral r) = StrLiteral (if l /= r then "TRUE" else "FALSE")

dispatchFn :: String -> LogoEvaluator LogoToken
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
