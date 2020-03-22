module BoundedDouble

import Data.So

data BoundedDouble : (a, b : Double) -> Type where
    MkBoundedDouble : (x : Double)
                      -> {auto rightSize : So (a <= b)}
                      -> {auto leftId : So (a <= a)}
                      -> {auto rightId : So (b <= b)}
                      -> {auto high : So (a <= x)}
                      -> {auto low : So (x <= b)}
                      -> BoundedDouble a b
    -- Min : {auto rightSize : So (a <= b)} -> BoundedDouble a b

Num (BoundedDouble a b) where
    (+) (MkBoundedDouble u) (MkBoundedDouble v) =
      let x = u + v
       in case (choose (a <= x), choose (x <= b)) of
               (Left _, Left _) => MkBoundedDouble x
               (Right _, _) => MkBoundedDouble a
               (_, Right _) => MkBoundedDouble b

    (*) (MkBoundedDouble u) (MkBoundedDouble v) =
      let x = u * v
       in case (choose (a <= x), choose (x <= b)) of
               (Left _, Left _) => MkBoundedDouble x
               (Right _, _) => MkBoundedDouble a
               (_, Right _) => MkBoundedDouble b

    fromInteger from = ?rhs


Neg (BoundedDouble a b) where
  negate (MkBoundedDouble u) =
    let x = negate u
     in case (choose (a <= x), choose (x <= b)) of
             (Left _, Left _) => MkBoundedDouble x
             (Right _, _) => MkBoundedDouble a
             (_, Right _) => MkBoundedDouble b

  (-) (MkBoundedDouble u) (MkBoundedDouble v) =
    let x = u - v
     in case (choose (a <= x), choose (x <= b)) of
             (Left _, Left _) => MkBoundedDouble x
             (Right _, _) => MkBoundedDouble a
             (_, Right _) => MkBoundedDouble b



{-
red : BoundedDouble 0.0 1.0
red = MkBoundedDouble 0.5

green : BoundedDouble 0.0 1.0
green = red * red - red
-}
