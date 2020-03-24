module SphereTest

import Sphere
import Ray
import Tuple
import Transform
import Intersection


-- Unit sphere
unit : Sphere
unit = MkSphere 1 (point 0 0 0) identity


-- intersecting a scaled sphere with a ray
test : Maybe (Double, Double)
test  = let r = MkRay (point 0 0 (-5)) (vector 0 0 1)
            s = record { transform = scaling 2 2 2 } unit
         in case intersect r s of
                 Nothing => Nothing
                 Just (i1, i2) => Just (t i1, t i2)


testNormalize : Bool
testNormalize = let p = point (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)
                    n = normalAt p unit
                 in n == vector (sqrt 3 / 3) (sqrt 3 / 3) (sqrt 3 / 3)

testTransformedNormal : Tuple
testTransformedNormal = let s = record { transform = translation 0 1 0 } unit
                            n = normalAt (point 0 0.70711 (-0.70711)) s
                         in n

testTransformedNormal2 : Bool
testTransformedNormal2 = let m = (scaling 2 0.5 1) * (rotation_z (pi / 5))
                             s = record { transform = m } unit
                             n = normalAt (point 0 (sqrt 2 / 2) (- sqrt 2 / 2)) s
                          in n == vector 0 0.97014 (-0.24254)
