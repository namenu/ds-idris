module Ray

import Tuple
import Mat4

%access export

record Ray where
  constructor MkRay
  origin : Tuple
  direction : Tuple


position : Double -> Ray -> Tuple
position t (MkRay origin direction) = origin <+> (t <#> direction)

transform : Mat4 -> Ray -> Ray
transform m (MkRay origin direction) =
  MkRay (m *. origin) (m *. direction)

{-
foo : Ray
foo = MkRay (point 2 3 4) (vector 1 0 0)
position foo (-1)
-}
