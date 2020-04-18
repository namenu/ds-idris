module Data.VectUtils

import Data.Vect

%access export

indices : (n : Nat) -> Vect n (Fin n)
indices Z = []
indices (S k) = FZ :: (map FS $ indices k)

zipi : Vect n a -> Vect n (Fin n, a)
zipi {n} m = zip (indices n) m


cartesianProduct : List a -> List b -> List (a, b)
cartesianProduct xs ys = concatMap (\x => map (\y => MkPair x y) ys) xs
