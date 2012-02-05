module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Logo.Turtle
import Logo.Types
import Logo.TokenParser
import Logo.Builtins
import Logo.Evaluator


main :: IO ()
main = defaultMain (stroke logo # lw 0.2)
 where
   fromRight (Right x) = x
   tokens = fromRight $ tokenize "repeat 5 [ fd 100 rt 144 ]"
   t = Turtle True 0 (Path [(P (0,0), Trail [] False)])
   ctx = LogoContext t builtins
   res = evaluateWithContext tokens ctx
   getLogoPath t@(Turtle _ _ p) =  p
   logo = case res of
            (_,LogoContext t _) -> getLogoPath t

