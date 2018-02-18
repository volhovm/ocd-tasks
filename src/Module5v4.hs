{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Collision algorithm, birthday paradox.

module Module5v4 () where

import Universum hiding (exp)

import System.Random (randomRIO)

import Lib (exp, findCollision)

----------------------------------------------------------------------------
-- 5.36
----------------------------------------------------------------------------

-- Probability of having two birthdays on the same day.
e536a1 :: Integer -> Double
e536a1 (fromIntegral -> n) = 1 - product (map (\i -> (365 - i) / 365) [0..n-1])

e536a2 :: Integer -> Double
e536a2 n = 1 - (364/365) ^ n

{-

(a)

λ> e536a1 23
0.5072972343239857
λ> e536a1 40
0.891231809817949

λ> e536a2 200
0.4222980433009368

(b) Well, it's like 1 - Π{i=1..n}(N-(i-1))/N

(c) P = 1 - Π(1 - (i-1)/N)
      ≥ 1 - Π(exp{(1-i)/N})
      = 1 - exp{(n - ∑{i=1..n}i)/N}
      = 1 - exp{(n - (n+1)n/2)/N}
      = 1 - exp{(n - n^2)/2N}
-}

----------------------------------------------------------------------------
-- 5.37
----------------------------------------------------------------------------

{-
(a) 1 - (51/52)^8 = 0.1438
(b) 1 - (44/52 * ... * 37/52) = 0.866
-}

----------------------------------------------------------------------------
-- 5.38
----------------------------------------------------------------------------

{-
(a) First of all, d(e^(-x))/dx = -e^(-x) is greater than d(1-x)/dx for all x>0:
-e^(-x) > -1
e^(-x) < 1
e^x > 1
x > 0

e^0 = 1, (1-x)(0) = 1, so in x = 0 they are equal, but exponent grows
faster. Since both are monotonic, we're done.

(b) f = (1-x)^a  + ax^2/2 - e^(-ax)
    f' = a(x - (1-x)^(a-1) + e^(-ax))
    f'' = a(1 + (a-1)(1-x)^(a-2) - a*e^(-ax))
    f''' = a(a^2*e(-ax) - (a-2)(a-1)(1-x)^(a-3))

f(0) = 0
f'(0) = 0,
f''(0) = 0,
f'''(0) = a(3a-2) > 1

Since we found non-zero value in odd power, 0 is not an maxima, but an
inflection point.

Great. Next to show is that f'(x) > 0 on x ∈ (0,1].

a(x - (1-x)^(a-1) + e^(-ax)) > 0
x - (1-x)^(a-1) + e^(-ax) > 0
x - (1-x)^(a-1) + (1-x)^a > 0
x - x * (1-x)^(a-1) > 0
x(1 - (1-x)^(a-1)) > 0
(1-x)^(a-1) < 1

which always holds.

Ok, so f(0) = 0, it's an inflection point, then f grows on x∈[0,1] for
all a. It means it doesn't have any more roots on this interval.

:handwaving:

(c) Is straightforward, just apply the (b) formula.
Regarding D = mn^2/(2N^2), if m = p*sqrt(N), n = q*sqrt(N), then
D = sp^2 / 2(sqrt(N)); let's say s = 2p, then D = p^3/sqrt(N), then
p can be taken really low.
-}

----------------------------------------------------------------------------
-- 5.39
----------------------------------------------------------------------------

dlpCollision :: Int -> Integer -> Integer -> Integer -> IO (Maybe Integer)
dlpCollision n p g h = do
    let genList :: IO [Integer]
        genList = replicateM n (randomRIO (0, p))
    as <- map (\y -> (exp p g y,y)) <$> genList
    bs <- map (\z -> ((h * exp p g z) `mod` p,z)) <$> genList
    let convert (y,z) = (y - z) `mod` p
    pure $ convert <$> findCollision as bs

{-
20^2 = 400, less than sqrt(811) = 20, but still:
λ> dlpCollision 20 811 10 106
Nothing
λ> dlpCollision 20 811 10 106
Nothing
λ> dlpCollision 20 811 10 106
Just 646
λ> dlpCollision 20 811 10 106
Just 646
-}
