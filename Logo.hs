module Main where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Logo.Turtle
import Logo.Types
import Logo.TokenParser
import Logo.Builtins
import Logo.Evaluator
import qualified Data.Map as M


main :: IO ()
main = defaultMain (stroke logo # lw 0.2)
 where
   fromRight (Right x) = x
   tokens = fromRight $ tokenize "to square :length repeat 4 [ fd :length rt 90 ] end repeat 36 [ square 50 rt 10 ]"
   t = Turtle True 0 (Path [(P (0,0), Trail [] False)])
   ctx = LogoContext t builtins (M.empty)
   res = evaluateWithContext tokens ctx
   getLogoPath t@(Turtle _ _ p) =  p
   logo = case res of
            (_,LogoContext t _ _) -> getLogoPath t

