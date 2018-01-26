module Data.RDP where

import Prelude hiding (abs, head, tail, init, last, length, (++))
import Data.Vector

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

argMax :: (Ord a) => Vector Vector3 -> (Vector3 -> a) -> (Int, a)
argMax vs f
  | length vs == 0 = (-1, f (Vector3 0 0 0))
  | length vs == 1 = (0, f (head vs))
  | otherwise      = argMax' 0 (f (head vs)) 1 (tail vs)
    where argMax' maxI maxV i vs' = if length vs' == 0 then
                                      (maxI, maxV)
                                    else
                                      if f (head vs') > maxV then
                                        argMax' i (f (head vs')) (i + 1) (tail vs')
                                      else
                                        argMax' maxI maxV (i + 1) (tail vs')

rdp :: Vector Vector3 -> Float -> Vector Vector3
rdp vs eps
  | length vs < 3 = vs
  | otherwise     = if dmax > eps then
                      (init part1) ++ part2
                    else
                      fromList [vFirst,vLast]
  where
    l = length vs
    vFirst = head vs
    vLast  = last vs
    vRest  = slice 1 (l - 2) vs
    (i, dmax) = argMax vRest (\v -> perpDist vFirst vLast v)
    part1 = slice 0 (i + 1) vRest
    part2 = slice i (l - i) vRest
    
