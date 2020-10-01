{-# LANGUAGE Arrows #-}
module Main where

import Graphics.UI.GLUT

type F = GLfloat

myPoints :: [(F, F, F)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]


main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Hello World"
  displayCallback $= display
  mainLoop



display :: DisplayCallback
display = do
  clear [ ColorBuffer ]
  renderObject Wireframe (Cube 0.5)
  flush
