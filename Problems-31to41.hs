--  Arithmetic

import Prelude hiding (gcd)

-- 2 Problem 31 
-- Determine whether a given integer number is prime. 
isPrime :: (Integral a) => a -> Bool
isPrime n | n < 4 = n /= 1 
isPrime n = all ((/=0) . mod n) $ takeWhile (<= m) candidates 
        where candidates = (2:3:[x + i | x <- [6,12..], i <- [-1,1]])
              m = floor . sqrt $ fromIntegral n
              
-- 3 Problem 32 
-- Determine the greatest common divisor of two positive integer numbers
gcd a b
  | b == 0 = abs a
  | otherwise = gcd b (a `mod` b)

-- 4 Problem 33
-- Determine whether two positive integer numbers are coprime. Two
-- numbers are coprime if their greatest common
-- divisor equals 1.
coprime a b = gcd a b == 1

--  5 Problem 34
-- Calculate Euler's totient function phi(m).
-- Euler's so-called totient function phi(m) is defined as the number
-- of positive integers r (1 <= r < m) that are coprime to m. 
totient n = length [x | x <- [1..n], coprime n x]    

-- 6 Problem 35

-- Determine the prime factors of a given positive integer. Construct
-- a flat list containing the prime factors in ascending order.
primes = filter isPrime [2..]

primeDivisors n = [d | d <- takeWhile (<=n) primes, n `rem` d == 0]

primeFactors n = case primeDivisors n of
  [] -> []
  (d:_) -> d : primeFactors (n `div` d)

-- Problem 36
-- Determine the prime factors of a given positive integer.
-- Construct a list containing the prime factors and their multiplicity.

