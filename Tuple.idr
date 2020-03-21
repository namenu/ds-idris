module Tuple

import Const

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


(+) : Tuple -> Tuple -> Tuple
(+) a b = MkTuple (x a + x b) (y a + y b) (z a + z b) (w a + w b)

(-) : Tuple -> Tuple -> Tuple
(-) a b = MkTuple (x a - x b) (y a - y b) (z a - z b) (w a - w b)

neg : Tuple -> Tuple
neg a = MkTuple (- x a) (- y a) (- z a) (- x a)


(*) : Tuple -> Double -> Tuple
(*) a s = MkTuple (x a * s) (y a * s) (z a * s) (w a * s)

(/) : Tuple -> Double -> Tuple
(/) a s = MkTuple (x a / s) (y a / s) (z a / s) (w a / s)

mag : Tuple -> Double
mag (MkTuple x y z w) = with Doubles sqrt (x * x + y * y + z * z + w * w)

normalize : Tuple -> Tuple
normalize a = a / (mag a)

dot : Tuple -> Tuple -> Double
dot a b = sqrt (x a * x b) + (y a * y b) + (z a * z b) + (w a * w b)

cross : Tuple -> Tuple -> Tuple
cross (MkTuple ax ay az aw) (MkTuple bx by bz bw) =
  vector (ay * bz - az * by)
         (az * bx - ax * bz)
         (ax * by - ay * bx)
