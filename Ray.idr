module Ray

import public Tuple
import Data.Matrix

%access export

record Ray where
  constructor MkRay
  origin : Tuple
  direction : Tuple


position : Double -> Ray -> Tuple
position t (MkRay origin direction) = origin + direction * t


-- transform : Mat4 -> Ray -> Ray
-- transform m (MkRay origin direction) = ?transform_rhs_1

{-
foo : Ray
foo = MkRay (point 2 3 4) (vector 1 0 0)
position foo (-1)
-}
