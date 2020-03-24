module Color

import Const

%access export

||| TODO : bound r, g, b to 0.0 ~ 1.0
record Color where
  constructor MkColor
  r, g, b : Double


black : Color
black = MkColor 0 0 0

Eq Color where
  (==) (MkColor r1 g1 b1) (MkColor r2 g2 b2)
         = abs (r1 - r2) < ε && abs (g1 - g2) < ε && abs (b1 - b2) < ε



(+) : Color -> Color -> Color
(+) (MkColor r1 g1 b1) (MkColor r2 g2 b2) = MkColor (r1 + r2) (g1 + g2) (b1 + b2)

(-) : Color -> Color -> Color
(-) (MkColor r1 g1 b1) (MkColor r2 g2 b2) = MkColor (r1 - r2) (g1 - g2) (b1 - b2)

(*) : Color -> Color -> Color
(*) (MkColor r1 g1 b1) (MkColor r2 g2 b2) = MkColor (r1 * r2) (g1 * g2) (b1 * b2)


-- Colorspace

infixl 5 .*
infixl 5 *.

(.*) : Double -> Color -> Color
(.*) v (MkColor r g b) = MkColor (v * r) (v * g) (v * b)

(*.) : Color -> Double -> Color
(*.) (MkColor r g b) v = MkColor (r * v) (g * v) (b * v)
