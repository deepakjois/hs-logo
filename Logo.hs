{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Applicative ((<$>))

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Path.Turtle

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
  let path = runTurtle $ evaluateSourceTokens tokens
      d = stroke path
  withArgs ["-o", o] $ defaultMain (d # lw (0.005 * width d) # centerXY # pad 1.1)

readSource :: FilePath -> IO [LogoToken]
readSource f = do
  tokens <- tokenize <$> readFile f
  case tokens of
    Left x -> error $ show x
    Right t -> return t

evaluateSourceTokens :: [LogoToken] -> Turtle ()
evaluateSourceTokens tokens = do
  let initialContext = LogoContext builtins M.empty M.empty
  res <- evaluateWithContext tokens initialContext
  case res of
    Left  err -> error $ show err
    Right _ -> return ()

