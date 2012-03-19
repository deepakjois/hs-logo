module Logo.Builtins.Control (controlBuiltins) where

import qualified Data.Map as M

import Control.Applicative ((<$>))

import Text.Parsec.Prim (modifyState, many)
import Text.Parsec.Combinator (manyTill)

import Logo.Types
import Logo.Evaluator

repeat_, repcount, for, dotimes, to, if_, ifelse :: [LogoToken] -> LogoEvaluator LogoToken

controlBuiltins :: M.Map String LogoFunctionDef
controlBuiltins = M.fromList
  [ ("repeat",   LogoFunctionDef 2 repeat_)
  , ("repcount", LogoFunctionDef 0 repcount)
  , ("for",      LogoFunctionDef 2 for)
  , ("dotimes",  LogoFunctionDef 2 dotimes)
  , ("to",       LogoFunctionDef 0 to)
  , ("if",       LogoFunctionDef 2 if_)
  , ("ifelse",   LogoFunctionDef 3 ifelse)
  ]

repeat_ (NumLiteral n : (t@(LogoList _) : [])) =
  repeatWithIterCount 1
 where
  repeatWithIterCount x
    | x > n    = return $ StrLiteral ""
    | otherwise = evaluateInLocalContext (M.fromList [("repcount", NumLiteral x)]) $ do
                    evalList t
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
                    evalList instructionList

for _ = error "Invalid arguments for function 'for'"

dotimes [ control@(LogoList _), instructionList@(LogoList _) ] = do
  mapM_ loop forList
  return $ StrLiteral ""
 where LogoList [Identifier v, NumLiteral times] = control
       forList = takeWhile (< times) $ iterate (+ 1) 0
       loop cur = evaluateInLocalContext (M.fromList [(v, NumLiteral cur)]) $
                    evalList instructionList

dotimes _ = error "Invalid arguments for dotimes"

if_ [StrLiteral val, ifList]
  | val == "TRUE"  = evalList ifList
  | val == "FALSE" = return $ StrLiteral ""

if_ _ = undefined

ifelse [StrLiteral val, ifList, elseList]
  | val == "TRUE"  = evalList ifList
  | val == "FALSE" = evalList elseList

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
createLogoFunction vars_ tokens_ args =
  evaluateInLocalContext (M.fromList $ zip vars_ args) $
    evaluateTokens tokens_
