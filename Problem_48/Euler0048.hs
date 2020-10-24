module Euler0048 where

import Data.List
import Test.QuickCheck

{-
  https://projecteuler.net/problem=48

  Time: 50 minutes

  The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

  Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

  Answer: 9110846700

-}

toFactor :: Integer -> Integer
toFactor 0 = 0
toFactor n = n^n + toFactor (n - 1)

toDigsList :: Integral a => a -> [a]
toDigsList 0 = []
toDigsList x = toDigsList (x `div` 10) ++ [x `mod` 10]

fromDigits :: [Integer] -> Integer
fromDigits = foldl addDigit 0
   where addDigit num d = 10 * num + d

euler48 :: Integer
euler48 = fromDigits $ reverse $ take 10 $ reverse $ toDigsList $ toFactor 1000


{- Test for the toFactor method -}
prop_increases :: Property
prop_increases = forAll (abs <$> arbitrary :: Gen Integer) (\n -> toFactor n < toFactor (n + 1))

test_toFactorIncrease :: IO Result
test_toFactorIncrease = quickCheckResult prop_increases

testReportEuler0048 :: IO Result
testReportEuler0048 = do
  print euler48
  test_toFactorIncrease
  
{-
  The output that is printed when you run the conciseTestReportEuler0048:

  λ> testReportEuler0048
    9110846700
    +++ OK, passed 100 tests.
    Success {numTests = 100, numDiscarded = 0, labels = fromList [([],100)], classes = fromList [], tables = fromList [], output = "+++ OK, passed 100 tests.\n"}
-}