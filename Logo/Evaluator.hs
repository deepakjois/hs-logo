module Logo.Evaluator where

import Logo.Types

import qualified Data.Map as M

import Control.Monad (replicateM)
import Control.Applicative ((<$>), (<|>))
import Control.Arrow ((&&&), (***))
import Control.Monad.Trans (lift)

import Text.Parsec.Prim (runParserT, tokenPrim, getState, putState, modifyState)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Text.Parsec.Error (ParseError)

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

evaluateWithContext :: [LogoToken] -> LogoContext -> TurtleIO (Either ParseError ([LogoToken], LogoContext))
evaluateWithContext tokens ctx = runParserT expression ctx "(stream)" tokens

evaluateTokens :: [LogoToken] -> LogoEvaluator LogoToken
evaluateTokens [] = return $ StrLiteral ""
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
multiplicativeExpression = parseWithOperators ["*", "/", "%"] powerExpression

powerExpression :: LogoEvaluator LogoToken
powerExpression = parseWithOperators ["^"] finalExpression

finalExpression :: LogoEvaluator LogoToken
finalExpression = do
  token <- anyLogoToken
  case token of
    Identifier s   -> dispatchFn s
    VarLiteral v   -> lookupVar v
    LogoExpr   e   -> do LogoList res <- evaluateTokens e
                         return $ head res
    _              -> return token

parseWithOperators :: [String] -> LogoEvaluator LogoToken  -> LogoEvaluator LogoToken
parseWithOperators operators parser = parser `chainl1` func
 where
  func  = do op <- choice $ map (logoToken . OperLiteral) operators
             return $ evalBinOp op

evalBinOp :: LogoToken -> LogoToken -> LogoToken -> LogoToken

-- Arithmetic
evalBinOp (OperLiteral "+") (NumLiteral l) (NumLiteral r) = NumLiteral (l + r)
evalBinOp (OperLiteral "-") (NumLiteral l) (NumLiteral r) = NumLiteral (l - r)
evalBinOp (OperLiteral "*") (NumLiteral l) (NumLiteral r) = NumLiteral (l * r)
evalBinOp (OperLiteral "/") (NumLiteral l) (NumLiteral r) = NumLiteral (l / r)
evalBinOp (OperLiteral "%") (NumLiteral l) (NumLiteral r) = NumLiteral $ fromIntegral ((truncate l `rem` truncate r) :: Integer )
evalBinOp (OperLiteral "^") (NumLiteral l) (NumLiteral r) = NumLiteral (l ** r)

-- Logical
evalBinOp (OperLiteral "<")  (NumLiteral l) (NumLiteral r) = StrLiteral (if l < r then "TRUE" else "FALSE")
evalBinOp (OperLiteral ">")  (NumLiteral l) (NumLiteral r) = StrLiteral (if l > r then "TRUE" else "FALSE")
evalBinOp (OperLiteral "=")  (NumLiteral l) (NumLiteral r) = StrLiteral (if l == r then "TRUE" else "FALSE")
evalBinOp (OperLiteral "<>") (NumLiteral l) (NumLiteral r) = StrLiteral (if l /= r then "TRUE" else "FALSE")
evalBinOp (OperLiteral "<=") (NumLiteral l) (NumLiteral r) = StrLiteral (if l <= r then "TRUE" else "FALSE")
evalBinOp (OperLiteral ">=") (NumLiteral l) (NumLiteral r) = StrLiteral (if l >= r then "TRUE" else "FALSE")

-- Undefined
evalBinOp op a b  = error $ "Evaluation undefined for " ++ show [op, a, b]

setLocals :: LogoSymbolTable -> LogoEvaluator ()
setLocals l = modifyState $ \s -> s { locals = l }

getLocals :: LogoEvaluator LogoSymbolTable
getLocals = locals <$> getState

evaluateInLocalContext :: LogoSymbolTable -> LogoEvaluator a -> LogoEvaluator a
evaluateInLocalContext localVars computation = do
  old <- getLocals
  setLocals $ localVars `M.union` old
  res <- computation
  setLocals old
  return res

lookupVar :: String -> LogoEvaluator LogoToken
lookupVar v = do
 (l,g) <-  (M.lookup v *** M.lookup v) . (locals &&& globals) <$> getState
 case l <|> g of
   Just x -> return x
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
