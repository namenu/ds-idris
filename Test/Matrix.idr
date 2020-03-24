module Test.Matrix

import Matrix
import Data.Vect

%access export  -- to make the test functions visible

-- "Test framework"
assert : String -> Bool -> IO ()
assert name b = do
  putStr name
  if b then putStrLn ": Passed"
  else putStrLn ": Failed"

assertEq : Eq a => String -> (given : a) -> (expected : a) -> IO ()
assertEq name g e = assert name (g == e)

assertNotEq : Eq a => String -> (given : a) -> (expected : a) -> IO ()
assertNotEq name g e = assert name (not(g == e))


-- Actual tests

m1 : Mat4
m1 = [[1,2,3,4],[5,6,7,8],[9,8,7,6],[5,4,3,2]]

m2 : Mat4
m2 = [[-2, 1, 2, 3],
      [3, 2, 1, -1],
      [4, 3, 6, 5],
      [1, 2, 7, 8]]

m3 : Matrix 3 3 Double
m3 = [[1,2,6],[-5,8,-4],[2,6,4]]

testDet : IO ()
testDet = let m3 = [[1,2,6],[-5,8,-4],[2,6,4]]
              m4 = [[-2,-8,3,5],[-3,1,7,3],[1,2,-9,6],[-6,7,7,-9]]
          in do assertEq "Test determinant 3x3" (det m3) (-196.0)
                assertEq "Test determinant 4x4" (det m4) (-4071.0)

testInv : IO ()
testInv = let m2 = [[8,-5,9,2],[7,5,6,1],[-6,0,9,6],[-3,0,-9,-4]]
              m2_expect = [[-0.15385, -0.15385, -0.28205, -0.53846 ],
                           [-0.07692,  0.12308,  0.02564,  0.03077 ],
                           [ 0.35897,  0.35897,  0.43590,  0.92308 ],
                           [-0.69231, -0.69231, -0.76923, -1.92308 ]]
           in assert "Test inverse" $ eq 0.00001 (inverse m2) m2_expect
