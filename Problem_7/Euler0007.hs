module Euler0007 where

import           Data.List
import           Data.Numbers.Primes

{-
  https://projecteuler.net/problem=7

  Time: 30 minutes

  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

  What is the 10 001st prime number?

  Answer: 104743

-}

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes'

primes' :: [Integer]
primes' = 2 : filter prime [3..]

get10001Prime:: Integer
get10001Prime = last $ take 10001 primes'

{-
    Q: Describe how you would test the functions you implement for the Euler
       exercise.
    A: There are a few things that can be tested.

    1. The answer of get10001Prime, needs to return a prime.
    2. The infinite list of primes, needs to only generate primes.

    A library is added: Data.Numbers.Primes.
    This way it is possible to check whether the implementation of functions is
    correct.

-}

testReportEuler007 :: IO ()
testReportEuler007 = do
    putStrLn "Is the 10001 prime a prime?"
    -- Test whether the 10001st prime is a prime by using the Data.Numbers.Primes package.
    print $ isPrime get10001Prime
    putStrLn "Are the first 10000 primes correctly generated?"
    print $ take 10000 primes == take 10000 primes'
    putStrLn "Are all the 10000 primes in the list of primes a prime?"
    -- Test it using the function of the Data.Numbers.Primes package.
    print $ all isPrime (take 10000 primes')
    putStrLn "What is the 10001st prime, according to this impl:"
    print get10001Prime
    putStrLn "Does that match the 10001st prime of the imported library?"
    print $ last $ take 10001 primes

{-
  The output that is printed when you run the testReportEuler007:

  Euler007> testReportEuler007
    Is the 10001 prime a prime?
    True
    Are the first 10000 primes correctly generated?
    True
    Are all the 10000 primes in the list of primes a prime?
    True
    What is the 10001st prime, according to this impl:
    104743
    Does that match the 10001st prime of the imported library?
-}


