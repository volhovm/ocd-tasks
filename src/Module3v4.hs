-- | Primality checks.

module Module3v4 where

import           Universum           hiding (exp)

import           Data.List           (nub)
import           Data.Numbers.Primes (isPrime, primeFactors, primes)
import           System.Random       (randomRIO)

import           Lib

----------------------------------------------------------------------------
-- 3.14 Carmichael test
----------------------------------------------------------------------------

{-
(a) First of all, if a^n = a (mod n), then obviously
a^n = a + qn = a + q p_1...p_n = a + (q{p_j})p_i = a (mod p_i)

So left-to-right is obvious.

Ok, now right-to-left.
What is the x that satisfies x = a^n (mod n)?
Let's solve these three congruences first: x = a^n (mod p_i).
We already know the answer, it's a. a = a^n (mod p_i).
Good, now let's combine it back: x = a (mod p_i).
Well, yeah, CRT answer is unique and it happens to be a.
-}
testCarmichael :: Integer -> Bool
testCarmichael n =
    not (isPrime n) &&
    (if coprimes ps && ordNub ps == ps
       then all satisfies ps
       else all (\a -> exp n a n == a) [1..n-1])
  where
    satisfies p = all (\a -> exp p a n == a) [1..p-1]
    ps = primeFactors n

e314 :: IO ()
e314 = print $ all testCarmichael [1729,10585,75361,1024651]
-- λ> e314
-- True

{-
(c) Let's suppose that n is even and prove it's not Carmichael. n = 2m.

Let's check (n-1).
((n-1)^2)^m = (n-1) (mod n) ?
(1)^m + 1 = 0 (mod n) ?
2 = 0 (mod n) ?
Well, not really.

(d) Suppose n = p^2 * (q1*q2...qs) = p * Q.

??? dunno as well

(e) ???
-}

----------------------------------------------------------------------------
-- 3.15 Miller-Rabin
----------------------------------------------------------------------------

traceT :: b -> a -> a
traceT _ = identity -- $ trace @Text

fermatTest :: Integer -> Integer -> Bool
fermatTest n a = exp n a n == a

-- | Checks if number is witness.
millerRabinTest :: Integer -> Integer -> Bool
millerRabinTest n a
  | a <= 1 = False
  | even n || gcd a n `notElem` [1,n] = traceT "A" True
  | otherwise = do
        let (k,kk) =
                fromMaybe (error "must exist") $
                find (\(_,v) -> (n - 1) `mod` v == 0 && odd ((n - 1) `div` v)) $
                iterate (\(k,v) -> (k+1,v*2)) (1,2)
        let q = (n - 1) `div` kk
        let ainit = exp n a q
        if ainit == 1
            then traceT "B" False
            else traceT "C" $
                 not $ any (== (n-1)) $ take k $
                 iterate (\a -> a * a `mod` n) ainit

-- | Does n iterations of Miller-Rabin.
millerRabinRandom :: Int -> Integer -> IO Bool
millerRabinRandom iter n =
    fmap or $
    replicateM iter $ do
        v <- randomRIO (2, n-1)
        let res = millerRabinTest n v
        --when (not res) $ print $ show v <> " is not a witness for " <> show n
        pure res

e315 :: IO ()
e315 = do
    print =<< millerRabinRandom 10 1105
    print =<< millerRabinRandom 10 294409
    print =<< millerRabinRandom 10 294439
    print =<< millerRabinRandom 10 118901509
    print =<< millerRabinRandom 10 118901521
    print =<< millerRabinRandom 10 118901527
    print =<< millerRabinRandom 10 118915387

{-
λ> e315
True
True
False
False
True
False
True
-}

----------------------------------------------------------------------------
-- 3.16
----------------------------------------------------------------------------

-- todo


----------------------------------------------------------------------------
-- 3.17 π(X)
----------------------------------------------------------------------------

primesCount :: Integral n => n -> n
primesCount x = fromIntegral $ length $ takeWhile (<x) primes

primesLimit :: Double -> Double
primesLimit x = (fromIntegral $ primesCount (round x)) * (log x) / x

{-
It does, yes.

λ> primesLimit 200
1.2186129943060484
λ> primesLimit 1000
1.160502886868999
λ> primesLimit 10000
1.131950831715873
λ> primesLimit 100000
1.1043198105999443
λ> primesLimit 1000000
1.0844899477790797
λ> primesLimit 10000000
1.0711747889618228
-}

----------------------------------------------------------------------------
-- 3.18 Prime numbers modulo i (coding)
----------------------------------------------------------------------------

p1,p2 :: Integral n => n -> n
p1 x = fromIntegral $ length $ filter (\p -> p `mod` 4 == 1) $ takeWhile (<x) primes
p2 x = fromIntegral $ length $ filter (\p -> p `mod` 4 == 3) $ takeWhile (<x) primes

{-
λ> map p1 [10,25,100,200,500,1000]
[1,3,11,21,44,80]
λ> map p2 [10,25,100,200,500,1000]
[2,5,13,24,50,87]
-}

p12Ratio :: Integral n => n -> Double
p12Ratio x = fromIntegral (p1 x) / fromIntegral (p2 x)

{-
λ> map p12Ratio $ take 6 $ iterate (*10) 100
[0.8461538461538461
,0.9195402298850575
,0.9838449111470113
,0.9948003327787022
,0.9962616347083058
,0.9993441597121523]


> make a conjecture
So I don't need to prove it? Alright. p2 > p1. Limit seems to be 1-0.
-}


----------------------------------------------------------------------------
-- 3.19 Remembering continuous analysis
----------------------------------------------------------------------------

{-

(a):

P(N) = (π(3N/2) - π(N/2))/N = 1/2 * (3/ln(3N/2) - 1/ln(N/2)) =
     = 1/2 * ln(3/4*N^2) / (ln(3N/2)*ln(N/2)) =
     = ln(√3/2 * N) / (ln(3N/2)*ln(N/2))

lim {P(N) / (1/ln(N))} =
     = { since, by lhopital's rule, lim{ln(kN)/ln(sN)} = 1 }
     = lim { 1/ln(N/2) / 1/ln(N) } = 1.

(b):

It's 1/ln(X) as well.

P(c1,c2,N) = 1/(c2-c1) * (ln(c1^c2/c2^c1 * N^{c2-c1}) / (ln(c1*N) ln(c2*N))
           = { let c3 = root{c2-c1}[c1^c2/c2^c1]} }
           = 1 * ln(c3 * N) / (ln(c1*N) ln(c2*N))

which is asymptotically equal to 1/ln(N)

-}

----------------------------------------------------------------------------
-- 3.20 Probability of getting a prime modulo something
----------------------------------------------------------------------------

{-

(a) P(N) = { X is prime, chosen from (c1*N,c2*N) } ~= 1/ln(X)
    P2(N) = { X is prime, chosen from half of (c1*N,c2*N) } ~= 2/ln(X).

    It's trivial to prove.

(b) Well, you just divide P by 3, so you get 3/ln(N). But also every other
    divisor of (3) is even, so that's where you get (2) from.

(c) Probability is 2 times bigger than in (b) because you now only consider
    odd half of (b) elements.

(d) (e) : TODO

-}

----------------------------------------------------------------------------
-- 3.21 Logarithm integral function
----------------------------------------------------------------------------

{-

(a) Direct application of differentiation by parts.
u = 1/ln(t), v = t.
O(1) = 2/ln(2)

(b) Trivial l'hopital's rule application:

    Since d/dx (Li(X)) = 1/ln(x) (variable upper bound), we have:

    lim(Li(x)/(x/ln(x))) = lim{ (1/ln(x)) / (ln^2(x)/(ln(x)-1)) } =
                         = lim{ (ln(x) - 1) / ln(x) }
                         = 1

(c) Well, we only need to show that Li(x) is asymptotically bigger
    than O(√x ln(x)), else is trivial. Li(x) ~ x/ln(x), so we expect
    the following limit to be 0.

    lim{ (x/ln(x)) / (√x ln(x)) } = lim { √x / ln^2(x) } =
                                  = 0

    We know that every polynomial is asymptotically bigger than any logarithm.

-}
