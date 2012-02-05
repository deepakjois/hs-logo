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
main = defaultMain (stroke logo # lw 0.4)
 where
   fromRight (Right x) = x
   tokens = fromRight $ tokenize snowflake
   t = Turtle True 0 (Path [(P (0,0), Trail [] False)])
   ctx = LogoContext t builtins (M.empty)
   res = evaluateWithContext tokens ctx
   getLogoPath t@(Turtle _ _ p) =  p
   logo = case res of
            (_,LogoContext t _ _) -> getLogoPath t


snowflake = "\
\to side :size :level \
\ifelse :level = 0 \
\[ \
\fd :size \
\] \
\[ \
\side :size / 3 :level - 1 \
\lt 60 \
\side :size / 3 :level - 1 \
\rt 120 \
\side :size / 3 :level - 1 \
\lt 60 \
\side :size / 3 :level - 1 \
\] \
\end \
\lt 30 \
\repeat 3 [side 250 4 rt 120]"
