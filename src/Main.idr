module Main

import Data.Fin
import Data.Vect
import Data.VectUtils
import Tuple
import Canvas
import Color
import Sphere
import Ray
import Intersection
import Data.IOArray

import Debug.Trace


rayOrigin : Tuple
rayOrigin = point 0 0 (-5)

wallZ : Double
wallZ = 10

wallSize : Double
wallSize = 7.0

canvasPixels : Int
canvasPixels = 100

pixelSize : Double
pixelSize = wallSize / (cast canvasPixels)

half : Double
half = wallSize / 2

-- canvas : Canvas 100 100

shape : Sphere
shape = unitSphere

colorAt : Fin w -> Fin h -> Color
colorAt x y =
  let c = MkColor 1 0 0
      worldY = -half - pixelSize * (cast (finToNat y))
      worldX = -half + pixelSize * (cast (finToNat x))
      position = point worldX worldY wallZ
      r = MkRay rayOrigin (normalize (sub position rayOrigin))
      xs = intersect r shape
   in case hit xs of
           Just _ => c
           Nothing => black


draw : Canvas -> IO ()
draw cvs@(Pixels w h ar) =
  do let pixels = concat $ map (\y => map (\x => writePixel x y (colorAt x y) cvs)
                                          (indices w))
                               (indices h)
     sequence_ pixels


ca : IO Canvas
ca = canvas 100 100

main : IO ()
main = do c <- ca
          draw c
          c' <- toPPM c
          Right r <- writeFile "output.ppm" c'
                | Left err => pure ()
          pure r
