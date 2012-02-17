module Logo.Builtins where

import Logo.Types
import Logo.Evaluator

import qualified Data.Map as M

import Control.Applicative ((<$>))
import Control.Monad.Trans (lift)

import Text.Parsec.Prim (modifyState, many)
import Text.Parsec.Combinator (manyTill)

import Diagrams.TwoD.Path.Turtle

fd, bk, rt, lt, home, setxy, seth, pu, pd :: [LogoToken] -> LogoEvaluator LogoToken
repeat_, repcount, for, dotimes, to, if_, ifelse :: [LogoToken] -> LogoEvaluator LogoToken
sin_, cos_, tan_, arctan :: [LogoToken] -> LogoEvaluator LogoToken

fd (NumLiteral d : []) = do
  updateTurtle (forward d)
  return $ StrLiteral ""

fd args = error $ "Invalid arguments to fd" ++ show args

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

home [] = do
  updateTurtle (setPos (0,0))
  return $ StrLiteral ""

home _ = error "Invalid arguments to home"

setxy [NumLiteral x, NumLiteral y] = do
  updateTurtle (setPos (x,y))
  return $ StrLiteral ""

setxy _ = error "Invalid arguments to setxy"

seth [NumLiteral n] = do
  updateTurtle (setHeading n)
  return $ StrLiteral ""

seth _ = error "Invalid arguments to seth"

pu [] = do
  updateTurtle penUp
  return $ StrLiteral ""

pu _ = error "Invalid arguments to pu"

pd [] = do
  updateTurtle penDown
  return $ StrLiteral ""

pd _ = error "Invalid arguments to pd"

repeat_ (NumLiteral n : (t@(LogoList _) : [])) =
  repeatWithIterCount 1
 where
  repeatWithIterCount x
    | x > n    = return $ StrLiteral ""
    | otherwise = evaluateInLocalContext (M.fromList [("repcount", NumLiteral x)]) $ do
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
createLogoFunction vars_ tokens_ args =
  evaluateInLocalContext (M.fromList $ zip vars_ args) $
    evaluateTokens tokens_

updateTurtle :: Turtle a  ->  LogoEvaluator a
updateTurtle = lift

sin_ [NumLiteral n] = return $ NumLiteral (sin $ fromDegrees n)
sin_ _ = error "Invalid arguments for sin"

cos_ [NumLiteral n] = return $ NumLiteral (cos $ fromDegrees n)
cos_ _ = error "Invalid arguments for cos"

tan_ [NumLiteral n] = return $ NumLiteral (tan $ fromDegrees n)
tan_ _ = error "Invalid arguments for cos"

arctan [NumLiteral n] = return $ NumLiteral (atan $ fromDegrees n)
arctan _ = error "Invalid arguments for cos"

fromDegrees :: Double -> Double
fromDegrees n = n * (pi/180)

builtins :: M.Map String LogoFunctionDef
builtins = M.fromList
  [ ("fd",       LogoFunctionDef 1 fd)
  , ("bk",       LogoFunctionDef 1 bk)
  , ("rt",       LogoFunctionDef 1 rt)
  , ("lt",       LogoFunctionDef 1 lt)
  , ("home",     LogoFunctionDef 0 home)
  , ("setxy",    LogoFunctionDef 2 setxy)
  , ("seth",     LogoFunctionDef 1 seth)
  , ("pu",       LogoFunctionDef 0 pu)
  , ("pd",       LogoFunctionDef 0 pd)
  , ("repeat",   LogoFunctionDef 2 repeat_)
  , ("repcount", LogoFunctionDef 0 repcount)
  , ("for",      LogoFunctionDef 2 for)
  , ("dotimes",  LogoFunctionDef 2 dotimes)
  , ("to",       LogoFunctionDef 0 to)
  , ("if",       LogoFunctionDef 2 if_)
  , ("ifelse",   LogoFunctionDef 3 ifelse)
  , ("sin",      LogoFunctionDef 1 sin_)
  , ("cos",      LogoFunctionDef 1 cos_)
  , ("tan",      LogoFunctionDef 1 tan_)
  , ("atan",     LogoFunctionDef 1 arctan)
  ]