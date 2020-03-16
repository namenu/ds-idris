module Data.Matrix

import Data.Vect

%default total


||| this alias trick doesn't work
-- NegNum : Type -> Type
-- NegNum a = (Num a, Neg a)

Matrix : Nat -> Nat -> Type -> Type
Matrix n m a = Vect n (Vect m a)


at : Fin n -> Fin m -> Matrix n m a -> a
at r c = index c . index r

row : Fin n -> Matrix n m a -> Vect m a
row = index

col : Fin m -> Matrix n m a -> Vect n a
col c = map (index c)


dotProduct : (Num a) => Vect n a -> Vect n a -> a
dotProduct x y = sum $ zipWith (*) x y


mult : Num a => Matrix n k a -> Matrix k m a -> Matrix n m a
mult m1 m2 = map (vectMult m2) m1
  where
    vectMult : Num a => Matrix n m a -> Vect n a -> Vect m a
    vectMult m v = map (dotProduct v) (transpose m)


submatrix : Fin (S n) -> Fin (S m) -> Matrix (S n) (S m) a -> Matrix n m a
submatrix r c = deleteRow r . deleteCol c
  where
    deleteRow : Fin (S n) -> Matrix (S n) m a -> Matrix n m a
    deleteRow = deleteAt

    deleteCol : Fin (S m) -> Matrix n (S m) a -> Matrix n m a
    deleteCol c = map (deleteAt c)


det2 : Neg a => Matrix 2 2 a -> a
det2 [[a, b], [c, d]] = a * d - b * c
--
-- determinant : (Num a, Neg a) => Matrix (S (S n)) (S (S n)) a -> a
-- determinant {n} m = case n of
--   Z => det2 m
--   (S k) => ?rhs
