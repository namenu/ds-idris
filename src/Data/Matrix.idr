module Data.Matrix

import public Data.Vect
import Data.Mod2
import Data.Bits

%default total
%access public export


-- Vect helpers

indices : (n : Nat) -> Vect n (Fin n)
indices Z = []
indices (S k) = FZ :: (map FS $ indices k)

zipi : Vect n a -> Vect n (Fin n, a)
zipi {n} m = zip (indices n) m


-- Constructor

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


(*) : Num a => Matrix n k a -> Matrix k m a -> Matrix n m a
(*) m1 m2 = map (vectMult m2) m1
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


-- private
det2 : Neg a => Matrix 2 2 a -> a
det2 [[a, b], [c, d]] = a * d - b * c


mutual
  ||| determinant
  det : Neg a => Matrix (2 + n) (2 + n) a -> a
  det {n=Z}     m = det2 m
  det {n=(S k)} m = sum $ map (\c => at 0 c m * cofactor 0 c m) $ indices (3 + k)

  cofactor : Neg a => Fin (3 + n) -> Fin (3 + n) -> Matrix (3 + n) (3 + n) a -> a
  cofactor r c m = let minor = det (submatrix r c m)
                       sign = case intToMod {n=1} (finToInteger r + finToInteger c) of
                                   MkMod2 (MkBits 0) => (* 1)  -- is even?
                                   _                 => (* -1) -- is odd?
                    in sign minor


invertible : (Ord a, Neg a) => a -> Matrix (2 + n) (2 + n) a -> Bool
invertible eps m = det m >= eps

-- private
inv2 : (Ord a, Neg a, Fractional a) => a -> Matrix 2 2 a -> Maybe (Matrix 2 2 a)
inv2 eps m@[[a00, a01], [a10, a11]]
  = let d = det m
     in case compare d eps of
             LT => Nothing
             _  => Just [[a00/d, a01/d], [a10/d, a11/d]]

inverse : (Ord a, Neg a, Fractional a) => a -> Matrix (2 + n) (2 + n) a -> Maybe (Matrix (2 + n) (2 + n) a)
inverse eps {n=Z}     m = inv2 eps m
inverse eps {n=(S k)} m = let d = det m
                           in case compare d eps of
                                   LT => Nothing
                                   _  => Just (map (\(i, row) =>
                                                 map (\(j, v) => let c = cofactor j i m
                                                                  in c / d)
                                                 (zipi row))
                                               (zipi m))
