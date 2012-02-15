module Logo.Evaluator where

import Logo.Types

import qualified Data.Map as M

import Control.Monad (replicateM)
import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)

import Text.Parsec.Prim (runParserT, tokenPrim, getState, putState, modifyState)
import Text.Parsec.Combinator (many1, option, choice)
import Text.Parsec.Error (ParseError)

import Diagrams.TwoD.Path.Turtle (Turtle)

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

evaluateWithContext :: [LogoToken] -> LogoContext -> Turtle (Either ParseError ([LogoToken], LogoContext))
evaluateWithContext tokens ctx = runParserT expression ctx "(stream)" tokens

evaluateTokens :: [LogoToken] -> LogoEvaluator LogoToken
evaluateTokens tokens = do
  ctx <- getState

  (t,s) <- lift $ do
      res <- evaluateWithContext tokens ctx
      case res of
        Left  e -> error $ show e
        Right r -> return r
  putState s
  return $ LogoList t

evaluateList :: LogoToken ->  LogoEvaluator LogoToken
evaluateList (LogoList l) = evaluateTokens l
evaluateList _            = undefined

satisfy ::  (LogoToken -> Bool) -> LogoEvaluator LogoToken
satisfy f =
  tokenPrim (\c -> show [c])
            (\pos _ _ ->  pos)
            (\c -> if f c then Just c else Nothing)

logoToken :: LogoToken -> LogoEvaluator LogoToken
logoToken x = satisfy (==x)

anyLogoToken :: LogoEvaluator LogoToken
anyLogoToken = satisfy (const True)

expression :: LogoEvaluator ([LogoToken], LogoContext)
expression =  do
  t <- many1 relationalExpression
  s <- getState
  return (t,s)

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
    VarLiteral v   -> lookupVar v
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
eval (OperLiteral "<=") (NumLiteral l) (NumLiteral r) = StrLiteral (if l <= r then "TRUE" else "FALSE")
eval (OperLiteral ">=") (NumLiteral l) (NumLiteral r) = StrLiteral (if l >= r then "TRUE" else "FALSE")

-- Undefined
eval op a b  = error $ "Evaluation undefined for " ++ show [op, a, b]

-- FIXME for now this sets a global var. Fix this after fixing issue #14
setLocalVar :: String -> LogoToken -> LogoEvaluator ()
setLocalVar k v = modifyState $ \s -> s { vars = M.insert k v $ vars s }


setGlobalVar :: String -> LogoToken -> LogoEvaluator ()
setGlobalVar k v = modifyState $ \s -> s { vars = M.insert k v $ vars s }

lookupVar :: String -> LogoEvaluator LogoToken
lookupVar v = do
 var <- (M.lookup v . vars) <$> getState
 case var of
   Just t -> return t
   _      -> error $ "variable " ++ v ++ " not in scope"

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
  arguments <- replicateM a relationalExpression
  -- call function and update context
  func arguments
