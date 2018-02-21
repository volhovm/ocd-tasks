{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Pollard's ρ method.

module Module5v5 (pollardRho) where

import Universum hiding (exp)

import Data.List (nub)

import Lib (exEucl, exp, inverseP)

----------------------------------------------------------------------------
-- 5.40
----------------------------------------------------------------------------

data PollardAcc = PollardAcc
    { paA :: !Integer
    , paB :: !Integer
    , paG :: !Integer
    , paD :: !Integer
    } deriving Show

pollardAfter :: PollardAcc -> Integer -> Integer -> Integer -> Integer
pollardAfter PollardAcc{..} p g h =
    case exEucl v (p-1) of
        (1,_,_) -> u * (inverseP p v)
        (d,s,_) -> let w = (u * s) `mod` (p-1)
                       r = (p-1) `div` d
                   in fromMaybe (error "pollardAfter") $
                      find (\e -> exp p g e == h) $
                      map (\k -> w `div` d + k * r) [0..d-1]
  where
    u = (paA - paG) `mod` (p-1)
    v = (paD - paB) `mod` (p-1)

{-
λ> pollardAfter (PollardAcc 81756 9527 67782 28637) 81799 11 41387
64857
λ> logDShank 81799 11 41387
64857

Must be correct.
-}

----------------------------------------------------------------------------
-- 5.41
----------------------------------------------------------------------------

pollardRho :: Integer -> Integer -> Integer -> Integer
pollardRho p g h = go (0 :: Integer) 1 1 $ PollardAcc 0 0 0 0
  where
    go i x y acc | x == y && i > 0 = pollardAfter acc p g h
    go i x y PollardAcc{..} = do
        let (x',a',b') = pFoo x paA paB
        let (y',g',d') =
                let (m1, m2, m3) = pFoo y paG paD -- uncurryN?
                in pFoo m1 m2 m3
        go (i+1) x' y' $ PollardAcc a' b' g' d'

    p1 = p `div` 3
    p2 = 2 * p1
    pFoo x a b
        | x < p1 = ( (g * x) `mod` p
                   , (a+1) `mod` (p-1)
                   , b)
        | x < p2 = ( (x * x) `mod` p
                   , (2*a) `mod` (p-1)
                   , (2*b) `mod` (p-1))
        | otherwise = ( (h*x) `mod` p
                      , a
                      , (b+1) `mod` (p-1))

{-
λ> pollardRho 7963 7 3018
5238
-}

----------------------------------------------------------------------------
-- 5.42
----------------------------------------------------------------------------

{-
λ> pollardRho 5011 2 2495
3351
λ> pollardRho 17959 17 14226
14557
λ> pollardRho 15239131 29 5953042
2528453
-}

----------------------------------------------------------------------------
-- 5.43
----------------------------------------------------------------------------

{-
Well, indeed, you switch to polar coordinates, e^(-(x^2/2 + y^2/2))
simplifies, and then you obtain I^2 = π/2, which yields the needed
result. (made on paper)
-}

----------------------------------------------------------------------------
-- 5.44
----------------------------------------------------------------------------

{-
(a) We have the forward orbit X={x_i}, where each element is taken
modulo N, but let's also follow Y={y_i} where y_i = x_i mod p, where p
is the smallest prime divisor of N. Forward orbit {x_i} has parameters
T and M, but notice, that since p << N, it might have its own suborbit
which is smaller than (T,M).

First of all, Y has _at least_ the same params as X, because if x_i =
y_i (mod N), then it's also true (mod p). Since there are less
elements in F_p, we can assume that Y has params (T',M'), where M' | M
and T' ≤ T (first collision might happen at most at T + M').

We state that by applying the same logic we did to estimate collisions
in N-ring, we'll have first collision in F_p in sqrt(p) steps. By
checking gcd(x-y,N) /= 1 on every step we will locate the first
collision over all divisors of N. Since p is the first divisor, it'll
be the one with higher probability. The first encountered gcd(x-y,N) =
d /= 1 can be resolved in two ways. Either d == N, thus we achieved
nothing (and we should repeat the whole procedure with another x_0),
or d | N, then we've found some divisor of N, most probably p.
-}

pollardRhoFactor :: Integer -> Integer -> Maybe (Integer,Double)
pollardRhoFactor n init = go (0 :: Integer) init init
  where
    f x = (x * x - 2) `mod` n
    go i x y =
        let x' = f x
            y' = f (f y)
            d = gcd (abs $ x' - y') n
        in if | d == 1 -> go (i+1) x' y'
              | d == n -> Nothing
              | otherwise -> Just $ (n `div` d,fromIntegral i / sqrt (fromIntegral n))

{-
(b)
λ> pollardRhoFactor 2201
Just (71,6.394568344792313e-2)
λ> pollardRhoFactor 9409613
Just (17393,1.0757913769276532e-2)
λ> pollardRhoFactor 1782886219
Just (224743,2.9603850270054325e-3)

(c)
λ> pollardRhoFactor 2201
Nothing
λ> pollardRhoFactor 9409613
Just (17393,1.6299869347388687e-3)
λ> pollardRhoFactor 1782886219
Just (224743,1.5867663744749119e-3)

Yes, running time change, they're less.

(d) Well, sequence Y = X (see (a)) and the first match we'll get will
be exactly x_i = x_j mod P, so gcd will be 1 and the function will
return Nothing.

(e) First of all, f x = x^2 is not a pseudorandom function, so sqrt(N)
estimate doesn't apply. Even more, it's actually decreasing the X
space size, since for every x = y^(2^i) there are 2^i roots. Also
there's a probability that at some point sequence will stuck at
{..,1,1,1..}.

For example, let's take prime 10061 and check how many different
orbits can we find:

λ> ordNub $ sort $ map (fst . findLoopSquare 10061) [1..10060]
[0,1,3,250,1003]
λ> map length $ group $ sort $ map (fst . findLoopSquare 10061) [1..10060]
[2,2,16,2008,8032]

We have 5 orbits, only two of which are popular.

Compared to x^2+1 (I've changed f inside):

λ> ordNub $ sort $ map (fst . findLoopSquare 10061) [1..10060]
[1,3,5,11,17,23,29,35,41,47,53,58,59,65,71,77,83,89,95,101,107,113,117,119,125,131,137,143,149,176]
λ> map length $ group $ sort $ map (fst . findLoopSquare 10061) [1..10060]
[6,6,46,92,144,104,94,116,90,54,122,4816,138,118,56,42,40,60,118,152,182,226,2572,252,254,76,40,30,2,12]

It has less shorter orbits.

In general, it works with x^2. Though, slower (the more orbits we
have, the shorter they are?..). Also, if you (in rare case) find an
orbit that gives you gcd = n, then switching orbit may be hard, you'll
get the same one with a very high probability.

-}

findLoopSquare :: Integer -> Integer -> (Integer,Integer)
findLoopSquare n init = go (0 :: Integer) init init
  where
    f x = (x * x - 2) `mod` n
    go i x y = let x' = f x
                   y' = f (f y)
               in if x' == y' then (i,x') else go (i+1) x' y'

{-

(f)

Let's apply the same test as in (e):

λ> ordNub $ sort $ map (fst . findLoopSquare 10061) [1..10060]
[0,1,2,5,6,11,20,83,250,1003]
λ> map length $ group $ sort $ map (fst . findLoopSquare 10061) [1..10060]
[4,8,6,12,126,96,252,4536,1004,4016]

So yes, it's much less random than x^2+1. Why?

Indeed, it's easy to prove f^n(u+u^-1) = u^(2^n) + u^(2^(-n)): every
time we expand (u1+u2)^2, the middle 2u1u2 is 2, minus 2 from f is 0.

How does it help? u + u^-1 = (u^2 + 1) / u. For some reason, if we map
all elements from F_p with v(u) = (u^2 + 1) / u, we get p/2+1 distinct
results. What is the inverse of v?

su = u^2 + 1 is a quadratic equation with roots: u = (s +- sqrt(s^2 -
4))/2. As we know, every second power of generator is a quadratic
residue, so half of group elements have roots.  We want s^2 - 4 = k^2
for some k, so s = sqrt(k^2 + 4). The distribution of k is uniform --
there are p/2 distinct k^2, (+4) only shifts it, so there must be p/2
valid candidates for s.

-}

e544 :: Integer -> [Integer]
e544 p = nub $ map (\u -> (((u * u + 1) `mod` p) * inverseP u p) `mod` p) [1..p-1]
