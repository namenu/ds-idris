module Mat4

import public Data.Matrix
import Const
import Tuple

%access public export


Mat4 : Type
Mat4 = Matrix 4 4 Double


identity : Mat4
identity = [[1, 0, 0, 0],
            [0, 1, 0, 0],
            [0, 0, 1, 0],
            [0, 0, 0, 1]]

namespace inv
  inverse : Mat4 -> Mat4
  inverse m = case inverse ε m of
                   Just m' => m'
                   Nothing => identity

-- (!) pattern matching makes type checker run indefinitely
-- multTuple : Mat4 -> Tuple -> Tuple
-- multTuple [[a00,a01,a02,a03],
--            [a10,a11,a12,a13],
--            [a20,a21,a22,a23],
--            [a30,a31,a32,a33]]
--           (MkTuple bx by bz bw)
--   = let x = a00 * bx + a01 * by + a02 * bz + a03 * bw
--         y = a10 * bx + a11 * by + a12 * bz + a13 * bw
--         z = a20 * bx + a21 * by + a22 * bz + a23 * bw
--         w = a30 * bx + a31 * by + a32 * bz + a33 * bw
--      in MkTuple x y z w

infixl 9 *.

(*.) : Mat4 -> Tuple -> Tuple
(*.) [r1, r2, r3, r4] v
  = let x = r1 . v
        y = r2 . v
        z = r3 . v
        w = r4 . v
     in MkTuple x y z w
        where
          (.) : Vect 4 Double -> Tuple -> Double
          (.) [ax, ay, az, aw] (MkTuple bx by bz bw) = ax * bx + ay * by + az * bz + aw * bw


eq : Matrix n m Double -> Matrix n m Double -> Bool
eq a b = let zs = zipWith (\l, r => abs (l - r)) (concat a) (concat b)
              in not $ elemBy (<) ε zs
