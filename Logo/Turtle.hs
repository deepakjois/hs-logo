module Logo.Turtle where

import Logo.Types
import Diagrams.Prelude


logoseg :: (Segment R2) -> Turtle -> Turtle
logoseg seg (Turtle d ang p) =
   Turtle d ang $ (modifyTrail  (\(Trail xs c) -> Trail (rotate ang seg:xs) c) p)

modifyTrail :: (Trail v -> Trail v) -> Path v -> Path v
modifyTrail f (Path ((p, t) : ps)) = Path $ (p, f t) : ps
modifyTrail _ p = p

-- Motion commands

-- | Move the turtle forward, along the current heading.
forward :: Double -> Turtle -> Turtle
forward x c = logoseg (Linear (x,0)) c

