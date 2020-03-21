module Intersection

import Ray
import Sphere
import Matrix

public export
record Intersection where
  constructor MkIntersection
  t : Double
  obj : Sphere


export
intersect : Ray -> Sphere -> Maybe (Intersection, Intersection)
intersect r s = let ray2 = transform (inverse (transform s)) r
                 in intersectRev ray2
                    where
                      intersectRev : Ray -> Maybe (Intersection, Intersection)
                      intersectRev (MkRay r_origin r_direction) =
                        let sphere_to_ray = r_origin - point 0 0 0

                            a = dot r_direction r_direction
                            b = 2 * dot r_direction sphere_to_ray
                            c = dot sphere_to_ray sphere_to_ray - 1

                            d = the Double (b * b - (4 * a * c))
                         in if d < 0 then Nothing
                                     else let t1 = (-b - sqrt d) / (2 * a)
                                              t2 = (-b + sqrt d) / (2 * a)
                                           in Just (MkIntersection t1 s, MkIntersection t2 s)