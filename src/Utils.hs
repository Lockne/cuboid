module Utils where

import Graphics.UI.GLUT hiding (Level, normalize)
import Types

p3dtoV3 :: RealFloat a => Point3D -> Vector3 a
p3dtoV3 (P3D x y z) = Vector3 (fromInteger x) (fromInteger y) (fromInteger z)

vectorApply :: (a -> b) -> Vector3 a -> Vector3 b
vectorApply f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

rotateVector :: (Integral a, RealFloat b) => a -> Vector3 b -> Vector3 b
rotateVector theta v = rotateTheta theta v
  where
    rotateTheta 0 (Vector3 x y z) = id $ Vector3 x y z
    rotateTheta 1 (Vector3 x y z) = Vector3 x z (-y)
    rotateTheta 2 (Vector3 x y z) = Vector3 x (-y) (-z)
    rotateTheta 3 (Vector3 x y z) = Vector3 x (-z) y
    rotateTheta i _ = rotateTheta (abs $ i `mod` 4) v

-- | (0 0 1) -> (0 1 0) -> (0 0 -1) -> (0 -1 0) -> (0 0 1)
