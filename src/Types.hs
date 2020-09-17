{-# LANGUAGE Arrows #-}
module Types where

import FRP.Yampa
import Graphics.UI.GLUT hiding (normalize, Level)

type R = GLdouble

data Point3D =
  P3D
    { x :: Integer
    , y :: Integer
    , z :: Integer
    }

data Level =
  Level
    { startPoint :: Point3D
    , endPoint :: Point3D
    , obstacles :: [Point3D]
    }

data Input =
  Keyboard
    { key :: Key
    , keyState :: KeyState
    , modifier :: Modifiers
    }

data ParsedInput =
  ParsedInput
    { wCount :: Double
    , aCount :: Double
    , sCount :: Double
    , dCount :: Double
    , upEvent :: Event Input
    , downEvent :: Event Input
    , leftEvent :: Event Input
    , rightEvent :: Event Input
    }
