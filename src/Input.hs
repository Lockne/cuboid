{-# LANGUAGE Arrows #-}

module Input where

import FRP.Yampa

import Types

import Graphics.UI.GLUT

-- | Event definitions
filterKeyDowns :: SF (Event Input) (Event Input)
filterKeyDowns = arr $ filterE ((== Down) . keyState)

keyIntegral :: Double -> SF (Event Input) Double
keyIntegral a = arr eventToSpeed >>> integral
  where
    eventToSpeed (Event _) = a
    eventToSpeed NoEvent = 0

-- | Input
parseInput :: SF (Event Input) ParsedInput
parseInput =
  proc i ->
  do down <- filterKeyDowns -< i
     wCount <- countKey 'w' -< down
     aCount <- countKey 'a' -< down
     sCount <- countKey 's' -< down
     dCount <- countKey 'd' -< down
     upEvent <- filterKey (SpecialKey KeyUp) -< down
     downEvent <- filterKey (SpecialKey KeyDown) -< down
     leftEvent <- filterKey (SpecialKey KeyLeft) -< down
     rightEvent <- filterKey (SpecialKey KeyLeft) -< down
     returnA -<
       ParsedInput wCount aCount sCount dCount upEvent downEvent leftEvent
         rightEvent
  where
    countKey c = filterE ((== (Char c)) . key) ^>> keyIntegral 1
    filterKey k = arr $ filterE ((== k) . key)
