module Data.RDP where

import Prelude hiding (abs)
  
data Vector3 = Vector3 Float Float Float
  deriving (Show)

dotp :: Vector3 -> Vector3 -> Float
dotp (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

abs :: Vector3 -> Float
abs v = sqrt (dotp v v)

sub :: Vector3 -> Vector3 -> Vector3
sub (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)

dist :: Vector3 -> Vector3 -> Float
dist v1 v2 = abs (sub v1 v2)

perpDist :: Vector3 -> Vector3 -> Vector3 -> Float
perpDist e1 e2 p = (dist e1 p) * sin theta
  where theta = acos cosTheta
        cosTheta = (dotp a b) / (abs a * abs b)
        a = sub p e1
        b = sub e2 e1
