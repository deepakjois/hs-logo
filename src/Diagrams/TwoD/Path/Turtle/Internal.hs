{-# LANGUAGE FlexibleContexts #-}
-- | A module consisting of core types and functions to represent and operate on
-- a \"turtle\".
--
-- More info about turtle graphics:
-- <http://en.wikipedia.org/wiki/Turtle_graphics>
--
-- The underlying graphics primitives are provided by the diagrams library
--
module Diagrams.TwoD.Path.Turtle.Internal
  (
    -- * Turtle data types and accessors
    Turtle(..), TurtlePath(..), PenStyle(..)

    -- * Motion commands
  , forward, backward, left, right

    -- * Pen style commands
  , setPenColor, setPenColour, setPenWidth

    -- * State setters
  , startTurtleState, setHeading, towards
  , setPenPos

    -- * Drawing control
  , penUp, penDown

    -- * Diagram related
  , getTurtleDiagram
  ) where

import Diagrams.Prelude
import Data.Colour hiding (atop)

-- | Style attributes associated with the turtle pen
data PenStyle = PenStyle
  { penWidth :: Double         -- ^ Width of pen. Default is 1.0
  , penColor :: Colour Double  -- ^ Color of pen. Default is @black@
  }

-- | Default pen style, with @penWidth@ set to 1.0 and @penColor@ set to black
defaultPenStyle :: PenStyle
defaultPenStyle = PenStyle 1.0 black

-- | Turtle path type that captures a list of paths and the style attributes
-- associated with them
data TurtlePath = TurtlePath
  { penStyle    :: PenStyle        -- ^ Style
  , turtleTrail :: (P2, Trail R2)  -- ^ Path
  }

-- | Core turtle data type. A turtle needs to keep track of its current
-- position, like its position, heading etc., and all the paths that it has
-- traversed so far.
--
-- We need to record a new path, everytime an attribute like style, pen position
-- etc changes, so that we can separately track styles for each portion of the
-- eventual path that the turtle took.
data Turtle = Turtle
  { -- | State of the pen. @False@ means that turtle movements will not draw
    -- anything
    isPenDown  :: Bool
     -- | Current position. This is updated everytime the turtle moves
  , penPos     :: P2
     -- | Orientation of the turtle in 2D space, given in degrees
  , heading    :: Deg
     -- | Path traversed by the turtle so far, without any style or pen
     -- attributes changing
  , currTrail  :: (P2, Trail R2)
     -- | Current style of the pen
  , currPenStyle  :: PenStyle
     -- | List of paths along with style information, traversed by the turtle
     -- previously
  , paths      :: [TurtlePath]
  }

-- | The initial state of turtle. The turtle is located at the origin, at an
-- orientation of 0 degrees with its pen position down. The pen style is
-- @defaultPenStyle@.
startTurtleState :: Turtle
startTurtleState = Turtle True origin 0 (origin, mempty) defaultPenStyle []

-- | Draw a segment along the turtle’s path and update its position. If the pen
-- is up, only the position is updated.
moveTurtle :: Segment R2  -- ^ Segment representing the path to travel
           -> Turtle      -- ^ Turtle to move
           -> Turtle      -- ^ Resulting turtle
moveTurtle s t@(Turtle pd pos h (o, Trail xs _) _ _) =
 if pd
   -- Add segment to current trail and update position
   then t { currTrail = (o, Trail newTrail False)
          , penPos = newPenPos
          }
   -- Update position only
   else t { penPos = newPenPos }
 where
   -- Rotate segment by orientation before adding to trail
   rotatedSeg  =  rotate h s
   newTrail    =  rotatedSeg : xs
   -- Calculate the new position along the segment
   newPenPos   =  flip fAtParam 1 . mkFixedSeg pos $ rotatedSeg

-- | Move the turtle forward by @x@ units
forward :: Double  -- ^ Distance to move
        -> Turtle  -- ^ Turtle to move
        -> Turtle  -- ^ Resulting turtle
forward x = moveTurtle (Linear $ r2 (x,0))

-- | Move the turtle backward by @x@ units
backward :: Double  -- ^ Distance to move
         -> Turtle  -- ^ Turtle to move
         -> Turtle  -- ^ Resulting turtle
backward x = moveTurtle (Linear $ r2 (negate x, 0))

-- | Turn the turtle by applying the given function to its current orientation
-- (in degrees)
turnTurtle :: (Deg -> Deg)  -- ^ Transformation to apply on current orientation
           -> Turtle        -- ^ Turtle to turn
           -> Turtle        -- ^ Resulting turtle
turnTurtle f t@(Turtle _ _ h _ _ _) = t { heading = f h  }

-- | Turn the turtle anti-clockwise (left)
left :: Double  -- ^ Degree of turn
     -> Turtle  -- ^ Turtle to turn
     -> Turtle  -- ^ Resulting turtle
left d = turnTurtle (+ (Deg d))

-- | Turn the turtle clockwise (right)
right :: Double  -- ^ Degree of turn
      -> Turtle  -- ^ Turtle to turn
      -> Turtle  -- ^ Resulting turtle
right d = turnTurtle (subtract (Deg d))

-- | Turn the turtle to the given orientation, in degrees
setHeading :: Double  -- ^ Degree of orientation
           -> Turtle  -- ^ Turtle to orient
           -> Turtle  -- ^ Resulting turtle
setHeading d = turnTurtle (const $ Deg d)

-- | Sets the turtle orientation towards a given location.
towards :: P2      -- ^ Point to orient turtle towards
        -> Turtle  -- ^ Turtle to orient
        -> Turtle  -- ^ Resulting turtle
towards p  = setHeading =<< (360 *) . (/ tau) . uncurry atan2 . unr2 . (p .-.) . penPos

-- Makes a "TurtlePath" from a "Turtle"’s @currTrail@ field
makeTurtlePath :: Turtle
               -> TurtlePath
makeTurtlePath t = TurtlePath (currPenStyle t) (currTrail t)

-- Returns a list of paths, with current trail added to a "Turtle"’s @paths@ field
addCurrTrailToPath :: Turtle
                   -> [TurtlePath]
addCurrTrailToPath t = if emptyTrail then paths t else makeTurtlePath t  : paths t
 where emptyTrail = (snd . currTrail) t == mempty

-- | Puts the turtle pen in “Up” mode. Turtle movements will not draw anything
--
-- Does nothing if the pen was already up. Otherwise, it creates a turtle with
-- the current trail added to @paths@.
penUp :: Turtle  -- ^ Turtle to modify
      -> Turtle  -- ^ Resulting turtle
penUp t = if isPenDown t
  then t { isPenDown = False, currTrail = newTrail, paths = newPaths }
  else t
 where newTrail   = (penPos t, mempty) -- Start a new empty trail
       newPaths   = addCurrTrailToPath t

-- | Puts the turtle pen in “Down” mode. Turtle movements will cause drawing to
-- happen
--
-- Does nothing if the pen was already down. Otherwise, starts a new trail
-- starting at the current position.
penDown :: Turtle  -- ^ Turtle to modify
        -> Turtle  -- ^ Resulting turtle
penDown t = if isPenDown t
  then t
  else t { isPenDown = True, currTrail = newTrail }
 where newTrail = (penPos t, mempty)

-- | Set the turtle X/Y position.
--
-- If pen is down and the current trail is non-empty, this will also add the
-- current trail to the @paths@ field.
setPenPos :: P2      -- ^ Position to place true
          -> Turtle  -- ^ Turtle to position
          -> Turtle  -- ^ Resulting turtle
setPenPos newPos t = if isPenDown t
  then t { penPos = newPos }
  else t { penPos = newPos, currTrail = newTrail, paths = newPaths }
 where newTrail = (newPos, mempty)
       newPaths = addCurrTrailToPath t


-- | Set a new pen width for turtle.
--
-- If pen is down, this adds the current trail to @paths@ and starts a new empty
-- trail.
setPenWidth :: Double -- ^ Width of Pen
            -> Turtle -- ^ Turtle to change
            -> Turtle -- ^ Resulting Turtle
setPenWidth d t = if isPenDown t
  then t { currPenStyle = newPenStyle, currTrail = newTrail, paths = newPaths }
  else t { currPenStyle = newPenStyle }
 where oldPenStyle = currPenStyle t
       newPenStyle = oldPenStyle { penWidth = d  }
       newTrail = (penPos t, mempty)
       newPaths = addCurrTrailToPath t

-- | Set a new pen color for turtle.
--
-- If pen is down, this adds the current trail to @paths@ and starts a new empty
-- trail.
setPenColour :: Colour Double -- ^ Width of Pen
             -> Turtle        -- ^ Turtle to change
             -> Turtle        -- ^ Resulting Turtle
setPenColour c t = if isPenDown t
  then t { currPenStyle = newPenStyle, currTrail = newTrail, paths = newPaths }
  else t { currPenStyle = newPenStyle }
 where oldPenStyle = currPenStyle t
       newPenStyle = oldPenStyle { penColor = c  }
       newTrail = (penPos t, mempty)
       newPaths = addCurrTrailToPath t

-- | alias of @setPenColour@
setPenColor :: Colour Double -- ^ Width of Pen
             -> Turtle        -- ^ Turtle to change
             -> Turtle        -- ^ Resulting Turtle
setPenColor = setPenColour

-- | Creates a diagram from a TurtlePath using the provided styles
turtlePathToStroke :: (Renderable (Path R2) b) => TurtlePath
                   -> (P2, Diagram b R2)
turtlePathToStroke (TurtlePath (PenStyle lineWidth_  lineColor_) (p,Trail xs _)) = (p,d)
 where d = lc lineColor_ .
           lw lineWidth_ .
           stroke $ pathFromTrail (Trail (reverse xs) False)

-- | Creates a diagram from a turtle
--
-- Applies the styles to each trails in @paths@ separately and combines them
-- into a single diagram
getTurtleDiagram :: (Renderable (Path R2) b) => Turtle
                 -> Diagram b R2
getTurtleDiagram t =
  position .
  map turtlePathToStroke .
  paths $ t # penUp -- Do a penUp to add @currTrail@ to @paths@