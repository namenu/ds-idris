module Color

record Color where
  constructor MkColor
  r, g, b : Double


black : Color
black = MkColor 0 0 0


(+) : Color -> Color -> Color
(+) (MkColor r1 g1 b1) (MkColor r2 g2 b2) = MkColor (r1 + r2) (g1 + g2) (b1 + b2)

(-) : Color -> Color -> Color
(-) (MkColor r1 g1 b1) (MkColor r2 g2 b2) = MkColor (r1 - r2) (g1 - g2) (b1 - b2)

(*) : Color -> Color -> Color
(*) (MkColor r1 g1 b1) (MkColor r2 g2 b2) = MkColor (r1 * r2) (g1 * g2) (b1 * b2)
