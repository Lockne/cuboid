{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows, BangPatterns, NamedFieldPuns #-}

module Update where

import Config
import FRP.Yampa
import Graphics (xAxis, yAxis, zAxis)
import Graphics.UI.GLUT hiding (Level, normalize)
import Input
import Types
import Utils

data WinLose
  = Win
  | Lose
  deriving (Eq)
