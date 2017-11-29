-- | Pollard's p-1 algorithm

module Module3v5 () where

import Universum hiding (exp)

import Data.Numbers.Primes (isPrime, primeFactors, primes)
import Lib (exp)

----------------------------------------------------------------------------
-- 3.22 Pollard's algorithm
----------------------------------------------------------------------------

pollardFactorPQ :: Integer -> Maybe Integer
pollardFactorPQ n = go 2 2
  where
    go :: Integer -> Integer -> Maybe Integer
    go _ j | j > 50 = Nothing
    go a0 j = do
        let a1 :: (Integral x) => x
            a1 = fromIntegral $ exp n a0 j
        let d = fromIntegral $ gcd (a1 - 1) n
        if d > 1 && d < n then Just d else go a1 (j+1)

{-
λ> map pollardFactorPQ [1739, 220459, 48356747]
[Just 37,Just 449,Just 6917]

js were: 6, 8, 19

λ> map primeFactors [1738, 220458, 48356746]
[[2,11,79],[2,3,7,29,181],[2,29,833737]]
-}

----------------------------------------------------------------------------
-- 3.23 Mersenne primes
----------------------------------------------------------------------------

mersennePrimes :: [Integer]
mersennePrimes = filter isPrime $ map (\x -> 2^x - 1) [2..]

{-
a,b)
λ> take 7 mersennePrimes
[3,7,31,127,8191,131071,524287]


c) Lemma: if 2^n - 1 = 1 (mod 3), then 2^{n+1} - 1 = 0 (mod 3)
It's the same that proving 2^n = 2 (mod 3) ⇒ 2^{n+1} = 1 (mod 3)
Actually if x = 2 (mod 3) then 2*x = 1 (mod 3). So that's obvious.

Now 2^2 = 4 is 1 mod 3, so even numbers are 1 mod 3, so 2^{even}-1 is 0 mod 3.
That's why it is not prime.

d) 2^(3m) = 8^m - 1, m > 2. Same applies, but for mod 7.
   Well, if x = 1 mod 7, then x * 8 = 1 mod 7, because 8 = 1.

f-g) well, i've read things on the internet...

-}
