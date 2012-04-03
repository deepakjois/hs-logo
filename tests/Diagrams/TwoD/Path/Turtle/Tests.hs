{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ViewPatterns  #-}
module Diagrams.TwoD.Path.Turtle.Tests
  ( tests
  ) where

import Control.Arrow ((***))

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Diagrams.Prelude
import Diagrams.TwoD.Path.Turtle.Internal

import Debug.Trace

tests =
  [ testProperty "Moves forward correctly" movesForward
  , testProperty "Moves backward correctly" movesBackward
  , testProperty "Moves backward and forward correctly" movesBackwardAndForward
  , testProperty "Moves left correctly" movesLeft
  , testProperty "Moves right correctly" movesRight
  , testProperty "Current trail is empty when pen is up" trailEmptyWhenPenUp
  ]

-- | The turtle moves forward by the right distance
movesForward :: Turtle
             -> Property
movesForward t =  isPenDown t ==>
     round diffPos      == round x  -- position is set correctly
  && round lenCurrTrail == round x  -- most recent trail has the right length
 where
  x            = 2.0
  t'           = t  # forward x
  diffPos      = magnitude $ penPos t' .-. penPos t
  lenCurrTrail = flip arcLength 0.0001 . head . trailSegments . snd . currTrail $ t'

-- | The turtle moves forward by the right distance
movesBackward :: Turtle
             -> Property
movesBackward t =  isPenDown t ==>
     round diffPos      == round x  -- position is set correctly
  && round lenCurrTrail == round x  -- most recent trail has the right length
 where
  x            = 2.0
  t'           = t  # backward x
  diffPos      = magnitude $ penPos t' .-. penPos t
  lenCurrTrail = flip arcLength 0.0001 . head . trailSegments . snd . currTrail $ t'

-- | The turtle moves forward and backward by the same distance and returns to
-- the same position
movesBackwardAndForward :: Turtle
                        -> Property
movesBackwardAndForward t = isPenDown t ==>
     abs(endX - startX) < 0.0001
  && abs(endY - startY) < 0.0001
  && totalSegmentsAdded == 2
 where
  x                          = 2.0
  t'                         = t # forward x # backward x
  (unp2 -> (startX, startY)) = penPos t
  (unp2 -> (endX, endY))     = penPos t'
  totalSegmentsAdded         = (uncurry (-)) . (getTrailLength *** getTrailLength) $ (t',t)
  getTrailLength             = (length . trailSegments . snd . currTrail)

-- | The turtle moves left four times and returns to the same position
movesLeft  :: Turtle
           -> Property
movesLeft t = isPenDown t ==>
     abs(endX - startX) < 0.0001
  && abs(endY - startY) < 0.0001
 where
  x                          = 2.0
  turn                       = 90
  t'                         = t # forward x # left turn
                                 # forward x # left turn
                                 # forward x # left turn
                                 # forward x
  (unp2 -> (startX, startY)) = penPos t
  (unp2 -> (endX, endY))     = penPos t'

-- | The turtle moves right four times and returns to the same position
movesRight  :: Turtle
            -> Property
movesRight t = isPenDown t ==>
     abs(endX - startX) < 0.0001
  && abs(endY - startY) < 0.0001
 where
  x                          = 2.0
  turn                       = 90
  t'                         = t # forward x # right turn
                                 # forward x # right turn
                                 # forward x # right turn
                                 # forward x
  (unp2 -> (startX, startY)) = penPos t
  (unp2 -> (endX, endY))     = penPos t'

-- | When the trail is empty, @currTrail@ always remains empty and no new paths
-- are added
trailEmptyWhenPenUp :: Turtle
                    -> Property
trailEmptyWhenPenUp t = isPenDown t ==> trailIsEmpty
 where
  t'           = t # penUp # forward 4 # backward 3
  trailIsEmpty = null . trailSegments . snd . currTrail $ t'

instance Show Turtle where
  show t@(Turtle a b c _ _ _) = show (a,b,c)


-- | Arbitrary instance for the Turtle type.
instance Arbitrary Turtle where
   arbitrary =
     Turtle <$> arbitrary
            <*> arbitrary
            <*> (Deg <$> arbitrary)
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

-- | Arbitrary instance for Diagrams type P2
instance Arbitrary P2 where
  arbitrary = p2 <$> arbitrary

-- | Arbitrary instance for TurtlePath
instance Arbitrary TurtlePath where
  arbitrary = TurtlePath <$> arbitrary <*> arbitrary

-- | Arbitrary instance of PenStyle
--
-- The color of the pen is chosen from black, blue or brown only
instance Arbitrary PenStyle where
  arbitrary = do
    penWidth_ <- arbitrary
    colorCode <- choose (1,3) :: Gen Int
    case colorCode of
      1 -> return $ PenStyle penWidth_  black
      2 -> return $ PenStyle penWidth_  blue
      3 -> return $ PenStyle penWidth_  brown

-- | Arbitrary instance of Segment
--
-- Currently this only generates linear segments only
instance Arbitrary (Segment R2)  where
  arbitrary = do
    h <- Deg <$> arbitrary
    x <- r2 <$> arbitrary
    return $ rotate h (Linear x)

instance Arbitrary (Trail R2) where
  arbitrary = Trail <$> arbitrary <*> (return False)
