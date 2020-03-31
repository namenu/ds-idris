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


-- test

eq : Matrix n m Double -> Matrix n m Double -> Bool
eq a b = let zs = zipWith (\l, r => abs (l - r)) (concat a) (concat b)
              in not $ elemBy (<) ε zs

m1 : Mat4
m1 = [[1,2,3,4],[5,6,7,8],[9,8,7,6],[5,4,3,2]]

m2 : Mat4
m2 = [[8,-5,9,2],[7,5,6,1],[-6,0,9,6],[-3,0,-9,-4]]

m3 : Matrix 2 2 Double
m3 = [[1, 2], [3,4]]

m2_expect : Mat4
m2_expect = [[-0.15385, -0.15385, -0.28205, -0.53846 ],
             [-0.07692,  0.12308,  0.02564,  0.03077 ],
             [ 0.35897,  0.35897,  0.43590,  0.92308 ],
             [-0.69231, -0.69231, -0.76923, -1.92308 ]]
