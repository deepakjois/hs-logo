module Logo.Turtle where

import Logo.Types
import Diagrams.Prelude


logoseg :: Segment R2 -> Turtle -> Turtle
logoseg seg (Turtle d ang p) =
   Turtle d ang
     (modifyTrail  (\(Trail xs c) -> Trail (rotate ang seg:xs) c) p)

modifyTrail :: (Trail v -> Trail v) -> Path v -> Path v
modifyTrail f (Path ((p, t) : ps)) = Path $ (p, f t) : ps
modifyTrail _ p = p

modifyAngle :: (Deg -> Deg) -> Turtle -> Turtle
modifyAngle f (Turtle d ang p)=  Turtle d (f ang) p

-- Motion commands

-- | Move the turtle forward, along the current heading.
forward, backward, left, right :: Double -> Turtle -> Turtle

forward x = logoseg (Linear (x,0))

-- | Move the turtle backward, directly away from the current heading.
backward x = logoseg (Linear (negate x, 0))

-- | Modify the current heading to the left by the specified amount.
left a = modifyAngle (+ Deg a)

-- | Modify the current heading to the right by the specified amount.
right a = modifyAngle (subtract (Deg a))
