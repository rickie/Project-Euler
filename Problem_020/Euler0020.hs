module Euler0020 where

import           Data.List

{-
  https://projecteuler.net/problem=20

  Time: 50 minutes

  n! means n × (n − 1) × ... × 3 × 2 × 1

  For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
  and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

  Find the sum of the digits in the number 100!

  Answer: 648

-}

fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n - 1)

-- We could have used package Data.Char method: DigitToInt.
-- Then the `digs` function would not have been necessary.
-- As a challenge, we choose to implement this function ourselves.
digs :: Integral a => a -> [a]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

euler20 :: Integer
euler20 = sum $ digs $ fac 100

{-
    Q: Describe how you would test the functions you implement for the Euler
       exercise.
    A: A random test would be possible for testing the factorial. An other
      option is to crosscheck it with an external library and see whether the
      things the implementation returns are correct.

      For the digs function, some tests are implemented underneath.
-}

testReportEuler0020 :: IO ()
testReportEuler0020 = do
    putStrLn "See what the function `digs` does to the number 123456789"
    print $ digs 123456789
    print "It should be equal to: [1,2,3,4,5,6,7,8,9]"
    putStrLn "A larger number then the max int: 2147483647, should return a correct list:"
    print $ digs 21474836479999
    putStrLn "It should return: [2,1,4,7,4,8,3,6,4,7,9,9,9,9]"
    putStrLn "--- The result of Euler 20 = "
    print euler20


{-
  The output that is printed when you run the conciseTestReportEuler0020:

  Euler0020> testReportEuler0020
    See what the function `digs` does to the number 123456789
    [1,2,3,4,5,6,7,8,9]
    "It should be equal to: [1,2,3,4,5,6,7,8,9]"
    A larger number then the max int: 2147483647, should return a correct list:
    [2,1,4,7,4,8,3,6,4,7,9,9,9,9]
    It should return: [2,1,4,7,4,8,3,6,4,7,9,9,9,9]
    --- The result of Euler 20 =
    648
-}



