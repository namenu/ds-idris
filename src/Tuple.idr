module Tuple

import Const
import public Control.Algebra
import public Control.Algebra.NumericImplementations
import public Control.Algebra.VectorSpace

%access export

public export
record Tuple where
  constructor MkTuple
  x, y, z, w : Double

%name Tuple a, b, c


point : Double -> Double -> Double -> Tuple
point x y z = MkTuple x y z 1.0

vector : Double -> Double -> Double -> Tuple
vector x y z = MkTuple x y z 0.0

Eq Tuple where
  (==) (MkTuple ax ay az aw) (MkTuple bx by bz bw)
         = abs (ax - bx) < ε && abs (ay - by) < ε && abs (az - bz) < ε && abs (aw - bw) < ε


add : Tuple -> Tuple -> Tuple
add a b = MkTuple (x a + x b) (y a + y b) (z a + z b) (w a + w b)

sub : Tuple -> Tuple -> Tuple
sub a b = MkTuple (x a - x b) (y a - y b) (z a - z b) (w a - w b)

neg : Tuple -> Tuple
neg a = MkTuple (- x a) (- y a) (- z a) (- x a)


mult : Tuple -> Double -> Tuple
mult a s = MkTuple (x a * s) (y a * s) (z a * s) (w a * s)

div : Tuple -> Double -> Tuple
div a s = MkTuple (x a / s) (y a / s) (z a / s) (w a / s)

mag : Tuple -> Double
mag (MkTuple x y z w) = with Doubles sqrt (x * x + y * y + z * z + w * w)

normalize : Tuple -> Tuple
normalize a = div a (mag a)

dot : Tuple -> Tuple -> Double
dot a b = sqrt (x a * x b) + (y a * y b) + (z a * z b) + (w a * w b)

cross : Tuple -> Tuple -> Tuple
cross (MkTuple ax ay az aw) (MkTuple bx by bz bw) =
  vector (ay * bz - az * by)
         (az * bx - ax * bz)
         (ax * by - ay * bx)


reflect : Tuple -> Tuple -> Tuple
reflect normal inv = let n = mult normal (2 * dot inv normal)
                      in sub inv n

{-
reflectTest : Bool
reflectTest = let v = vector 0 (-1) 0
                  n = vector (sqrt 2 / 2) (sqrt 2 / 2) 0
               in reflect n v == vector 1 0 0
-}


-- Algebraic structures

Semigroup Tuple where
  (<+>) = add

Monoid Tuple where
  neutral = MkTuple 0 0 0 0

Group Tuple where
  inverse = neg

AbelianGroup Tuple where {}


Module Double Tuple where
  (<#>) s v = mult v s

InnerProductSpace Double Tuple where
  (<||>) = dot
