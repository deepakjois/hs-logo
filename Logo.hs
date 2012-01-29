module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Logo.Turtle
import Logo.Types
import Logo.TokenParser
import Logo.Evaluator

main :: IO ()
main = defaultMain (stroke logo # lw 0.2)
 where
   fromRight (Right x) = x
   getLogoPath t@(Turtle _ _ p) =  p
   res = evaluate $ fromRight $ tokenize "fd 100"
   logo = case res of
            (_,LogoContext t _) -> getLogoPath t

