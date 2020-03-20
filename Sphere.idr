module Sphere

import public Tuple
import Data.Matrix

export
record Sphere where
  constructor MkSphere
  radius : Double
  origin : Tuple
