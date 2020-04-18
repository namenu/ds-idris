module Canvas

import Data.Vect
import Data.Fin
import Data.Buffer
import Data.Bits
import Color
import Data.IOArray
import Data.String.Extra


%access public export

data Canvas : Type where
  Pixels : (w : Nat) -> (h : Nat) -> (ar : IOArray Color) -> Canvas

linearize : Fin w -> Fin h -> Int
linearize {w} x y = cast ((finToNat y) * w + (finToNat x))

canvas : (w : Nat) -> (h : Nat) -> IO Canvas
canvas w h = do ar <- newArray (cast (w * h)) black
                pure $ Pixels w h ar

writePixel : Fin w -> Fin h -> Color -> Canvas -> IO ()
writePixel x y color (Pixels w h ar) = let pos = linearize x y
                                        in unsafeWriteArray ar pos color

pixelAt : Fin w -> Fin h -> Canvas -> IO Color
pixelAt x y (Pixels w h ar) = let pos = linearize x y
                               in unsafeReadArray ar pos


toInts : Canvas -> IO (List Int)
toInts (Pixels w h ar) = let sz = cast (w * h) - 1
                          in do cs <- traverse (unsafeReadArray ar) [0..sz]
                                pure $ concatMap colorToInts cs
                             where colorToInts : Color -> List Int
                                   colorToInts c = let (r, g, b) = toIntTuple c
                                                    in [r, g, b]

preamble : Canvas -> String
preamble (Pixels w h _) = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"

toPPM : Canvas -> IO String
toPPM c = do ints <- toInts c
             let content = unwords $ map show ints
             pure $ preamble c ++ content ++ "\n"
