module Graphics where

import FRP.Yampa

import Graphics.UI.GLUT hiding (Level)

import Types

import Utils

-- | Helpful GL constants for rotation
xAxis = Vector3 1 0 0 :: Vector3 R

yAxis = Vector3 0 1 0 :: Vector3 R

zAxis = Vector3 0 0 1 :: Vector3 R

-- | Colours
green = Color4 0.8 1.0 0.7 0.9 :: Color4 R

greenG = Color4 0.8 1.0 0.7 1.0 :: Color4 R

red = Color4 1.0 0.7 0.8 1.0 :: Color4 R

-- | rendering functions
renderShapeAt :: Object -> Level -> Vector3 R -> IO ()
renderShapeAt s l (Vector3 x y z) =
  preservingMatrix $ do
    translate $ Vector3 (0.5 - size' + x) (0.5 - size' + y) (0.5 - size' + z)
    renderObject Solid s
  where
    size' = (fromInteger $ size l) / 2

renderObstacle :: Level -> Vector3 R -> IO ()
renderObstacle l = (color green >>) . (renderShapeAt (Cube 1) l)

renderPlayer :: Level -> Vector3 R -> IO ()
renderPlayer l = (color red >>) . (renderShapeAt (Sphere' 0.5 20 20) l)

renderGoal :: Level -> Vector3 R -> IO ()
renderGoal l = (color greenG >>) . (renderShapeAt (Sphere' 0.5 20 20) l)

-- | Copied from reactive-glut
resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1)
resizeScene s@(Size width height) = do
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 45 (w2 / h2) 1 1000
  matrixMode $= Modelview 0
  where
    w2 = half width
    h2 = half height
    half z = realToFrac z / 2

initGL :: IO ()
initGL = do
  getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  createWindow "Cuboid!"
  depthFunc $= Just Less
  clearColor $= Color4 0 0 0 0
  light (Light 0) $= Enabled
  lighting $= Enabled
  lightModelAmbient $= Color4 0.5 0.5 0.5 1
  diffuse (Light 0) $= Color4 1 1 1 1
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  reshapeCallback $= Just resizeScene
  return ()

renderGame (Game l rotX pPos) = do
  loadIdentity
  translate $ Vector3 (0 :: R) 0 (-2 * (fromInteger $ size l))
    -- TODO: calculate rotation axis based on rotX/Y
  rotate (rotX * 10) xAxis
  color $ Color3 (1 :: R) 1 1
  position (Light 0) $= Vertex4 0 0 0 1
  renderObject Wireframe (Cube $ fromInteger $ size l)
  renderPlayer l pPos
  renderGoal l (p3dtoV3 $ endPoint l)
  mapM_ (renderObstacle l . p3dtoV3) $ obstacles l

draw :: SF GameState (IO ())
draw =
  arr $
  (\gs -> do
     clear [ColorBuffer, DepthBuffer]
     renderGame gs
     swapBuffers)
