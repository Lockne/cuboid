{-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import Control.Concurrent


type Pos = Double
type Vel = Double

fallingBall :: Pos -> Vel -> SF () (Pos, Vel)
fallingBall y0 v0 = proc () -> do
            v <- (v0 +) ^<< integral -< -9.81
            y <- (y0 +) ^<< integral -< v
            returnA -< (y,v)


main :: IO ()
main = reactimate init input output (fallingBall 1000 0)
     where init = return ()
           input = (\_ -> threadDelay 100000 >> return (0.1, Nothing))
           output = (\_ (pos, vel) -> putStrLn (concat ["pos: ", show pos, " vel: ", show vel]) >> return False)
