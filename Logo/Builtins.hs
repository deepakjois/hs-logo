module Logo.Builtins where

import Logo.Types
import Logo.Evaluator

import qualified Data.Map as M

import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)

import Text.Parsec.Prim (modifyState, many)
import Text.Parsec.Combinator (manyTill)

import Diagrams.TwoD.Path.Turtle

fd, bk, rt, lt, repeat_, repcount, for, dotimes, to, if_, ifelse :: [LogoToken] -> LogoEvaluator LogoToken

fd (NumLiteral d : []) = do
  updateTurtle (forward d)
  return $ StrLiteral ""

fd _ = error "Invalid arguments to fd"

bk (NumLiteral d : []) = do
  updateTurtle (backward d)
  return $ StrLiteral ""

bk _ = error "Invalid arguments to fd"

rt (NumLiteral a : []) = do
  updateTurtle (right a)
  return $ StrLiteral ""

rt _ = error "Invalid arguments to rt"

lt (NumLiteral a : []) = do
  updateTurtle (left a)
  return $ StrLiteral ""

lt _ = error "Invalid arguments to lt"

repeat_ (NumLiteral n : (t@(LogoList _) : [])) =
  repeatWithIterCount 1
 where
  repeatWithIterCount x
    | x > n    = return $ StrLiteral ""
    | otherwise = evaluateInLocalContext (M.fromList [("repcount", (NumLiteral x))]) $ do
                    evaluateList t
                    repeatWithIterCount (x + 1)

repeat_ _ = error "Invalid arguments for repeat"

repcount [] = do
  rc <- M.lookup "repcount" <$> getLocals
  case rc of
    Just c  -> return c
    Nothing -> error "repcount does not exist"

repcount _ = error "Invalid call to repcount"

for [ control@(LogoList _), instructionList@(LogoList _) ] = do
  mapM_ loop forList
  return $ StrLiteral ""
 where LogoList [Identifier v, NumLiteral start, NumLiteral end, NumLiteral step] = control
       forList = takeWhile withinBounds $ iterate (+ step) start
       withinBounds x = if step < 0 then x >= end else x <= end
       loop cur = evaluateInLocalContext (M.fromList [(v, NumLiteral cur)]) $
                    evaluateList instructionList

for _ = error "Invalid arguments for function 'for'"

dotimes [ control@(LogoList _), instructionList@(LogoList _) ] = do
  mapM_ loop forList
  return $ StrLiteral ""
 where LogoList [Identifier v, NumLiteral times] = control
       forList = takeWhile (< times) $ iterate (+ 1) 0
       loop cur = evaluateInLocalContext (M.fromList [(v, NumLiteral cur)]) $
                    evaluateList instructionList

dotimes _ = error "Invalid arguments for dotimes"

if_ [StrLiteral val, ifList]
  | val == "TRUE"  = evaluateList ifList
  | val == "FALSE" = return $ StrLiteral ""

if_ _ = undefined

ifelse [StrLiteral val, ifList, elseList]
  | val == "TRUE"  = evaluateList ifList
  | val == "FALSE" = evaluateList elseList

ifelse _ = error "Invalid arguments for if"

to [] = do
  (Identifier name) <- anyLogoToken
  args <- map fromVar <$> many (satisfy isVarLiteral)
  tokens <- manyTill anyLogoToken (logoToken $ Identifier "end")
  modifyState (addFunction name $ LogoFunctionDef (length args) (createLogoFunction args tokens))
  return $ StrLiteral ""
 where
  isVarLiteral (VarLiteral _) = True
  isVarLiteral _              = False

  fromVar (VarLiteral s)      = s
  fromVar _                   = undefined

  addFunction name fn ctx = ctx { functions = M.insert name fn (functions ctx) }

to _ = undefined

createLogoFunction ::  [String] -> [LogoToken] -> LogoFunction
createLogoFunction vars_ tokens_ = \args -> do
  evaluateInLocalContext (M.fromList $ zip vars_ args) $ do
    evaluateTokens tokens_

updateTurtle :: Turtle a  ->  LogoEvaluator a
updateTurtle = lift

builtins :: M.Map String LogoFunctionDef
builtins = M.fromList
  [ ("fd",       LogoFunctionDef 1 fd)
  , ("bk",       LogoFunctionDef 1 bk)
  , ("rt",       LogoFunctionDef 1 rt)
  , ("lt",       LogoFunctionDef 1 lt)
  , ("repeat",   LogoFunctionDef 2 repeat_)
  , ("repcount", LogoFunctionDef 0 repcount)
  , ("for",      LogoFunctionDef 2 for)
  , ("dotimes",  LogoFunctionDef 2 dotimes)
  , ("to",       LogoFunctionDef 0 to)
  , ("if",       LogoFunctionDef 2 if_)
  , ("ifelse",   LogoFunctionDef 3 ifelse)
  ]