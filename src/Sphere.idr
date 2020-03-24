module Sphere

import Tuple
import Matrix
import Material

%access public export


record Sphere where
  constructor MkSphere
  radius : Double
  origin : Tuple
  transform : Mat4
  material : Material


unitSphere : Sphere
unitSphere = MkSphere 1 (point 0 0 0) identity defaultMaterial

normalAtLocal : Tuple -> Sphere -> Tuple
normalAtLocal p sphere = normalize (p <-> point 0 0 0)

normalAt : Tuple -> Sphere -> Tuple
normalAt world_point (MkSphere _ _ s_transform _)
  = let object_point = (inverse s_transform) *. world_point
        object_normal = object_point <-> point 0 0 0
        world_normal = transpose (inverse s_transform) *. object_normal
     in normalize (record { w = 0 } world_normal)
