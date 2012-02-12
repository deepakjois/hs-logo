{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>))

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

import Logo.Types
import Logo.TokenParser
import Logo.Builtins
import Logo.Evaluator

import System.Environment (getProgName, withArgs)

import System.Console.CmdArgs.Implicit

import qualified Data.Map as M


data LogoOpts = LogoOpts
  { output :: String -- output file to write to
  , src    :: Maybe String -- source file to read from
  } deriving (Show, Data, Typeable)

logoOpts :: String -> LogoOpts
logoOpts prog = LogoOpts
  { output =  "logo.png"
           &= typFile
           &= help "Output image file (default=logo.png)"
  , src = def
        &= typFile
        &= args
  }
  &= summary "hs-logo Logo Interpreter v0.1"
  &= program prog

main :: IO ()
main = do
  prog <- getProgName
  opts <- cmdArgs (logoOpts prog)
  case src opts of
    Nothing -> error "Source file not specified"
    Just s  -> renderLogo s (output opts)

renderLogo :: String -> String -> IO ()
renderLogo s o = do
  tokens <- readSource s
  let path = evaluateSourceTokens tokens
  withArgs ["-o", o] $ defaultMain (stroke path # lw 0.4)

readSource :: FilePath -> IO [LogoToken]
readSource f = do
  tokens <- tokenize <$> readFile f
  case tokens of
    Left x -> error $ show x
    Right t -> return t

evaluateSourceTokens :: [LogoToken] -> Path R2
evaluateSourceTokens tokens = do
  let t                   = Turtle True 0 (Path [(P (0,0), Trail [] False)])
      initialContext      = LogoContext t builtins M.empty
  case evaluateWithContext tokens initialContext of
    (_,LogoContext (Turtle _ _ p) _ _) -> p
