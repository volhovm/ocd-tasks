-- | RSA basics

module Modulev3v2 where

import           Universum           hiding (exp)
import           Unsafe              (unsafeHead)

import qualified Data.HashSet        as HS
import           Data.List           (nub)
import           Data.Numbers.Primes (isPrime, primeFactors, primes)

import           Lib                 (crt, exp, inverse)

data SecKeyRSA = SecKeyRSA
    { rsP :: Integer
    , rsQ :: Integer
    , rsE :: Integer
    , rsD :: Integer
    } deriving Show

data PubKeyRSA = PubKeyRSA
    { rpN :: Integer
    , rpE :: Integer
    } deriving Show

toPublic :: SecKeyRSA -> PubKeyRSA
toPublic SecKeyRSA {..} = PubKeyRSA{..}
  where
    rpN = rsP * rsQ
    rpE = rsE

rsaEncrypt :: PubKeyRSA -> Integer -> Integer
rsaEncrypt PubKeyRSA{..} m = exp rpN m rpE

rsaDecrypt :: SecKeyRSA -> Integer -> Integer
rsaDecrypt SecKeyRSA{..} c = exp (rsP*rsQ) c rsD

----------------------------------------------------------------------------
-- 3.7 RSA examples
----------------------------------------------------------------------------

e37_pk = PubKeyRSA 2038667 103
e37_a = rsaEncrypt e37_pk 892383
e37_b = let rsP = 1301
            rsQ = rpN e37_pk `div` rsP
            rsE = rpE e37_pk
            rsD = inverse rsE $ (rsP-1)*(rsQ-1)
        in SecKeyRSA {..}
e37_c = rsaDecrypt e37_b 317730

{-
λ> e37_a
45293
λ> e37_b
SecKeyRSA {rsP = 1301, rsQ = 1567, rsE = 103, rsD = 810367}
λ> e37_c
514407
-}

----------------------------------------------------------------------------
-- 3.8 Too small N in RSA
----------------------------------------------------------------------------

e38_pk = PubKeyRSA 12191 37
e38_c = 587
e38_sk =
    let n = rpN e38_pk
        candidates = takeWhile (< round (sqrt $ fromInteger n)) primes
        rsP = fromMaybe (error "should exist") $ find (\p -> n `mod` p == 0) candidates
        rsQ = n `div` rsP
        rsE = rpE e37_pk
        rsD = inverse rsE $ (rsP-1)*(rsQ-1)
    in SecKeyRSA{..}
e38_m = rsaDecrypt e38_sk e38_c

{-
λ> e38_sk
SecKeyRSA {rsP = 73, rsQ = 167, rsE = 103, rsD = 8935}
λ> e38_m
10217
-}

----------------------------------------------------------------------------
-- 3.9 Quadratic polynomials
----------------------------------------------------------------------------

-- For given pq and (p-1)(q-1) solves for p and q.
findPQ :: Integer -> Integer -> Maybe (Integer,Integer)
findPQ pq k =
    if p*q /= pq || (p-1) * (q-1) /= k
    then Nothing
    else Just (p,q)
  where
    s = pq + 1 - k -- p+q
    d = s * s - 4 * pq
    p = (s + round (sqrt $ fromInteger d)) `div` 2
    q = (s - round (sqrt $ fromInteger d)) `div` 2

{-
λ> findPQ 352717 351520
(677,521)
λ> findPQ 77083921 77066212
(10007,7703)
λ> findPQ 109404161 109380612
(17183,6367)
λ> findPQ 172205490419 172204660344
(422183,407893)
-}

----------------------------------------------------------------------------
-- 3.10
----------------------------------------------------------------------------

{-
First thing to notice is that 2.8 says whenever we can find 4 roots of
x^2 = c (mod pq), we can factor pq.

Let's represent something in this form:
a^(de-1) = 1 (mod pq), but de-1 is actually even.
This is easy to see: both d and e are odd
(since gcd(d,(p-1)(q-1)) = 1 and (p-1)(q-1) is even).
So then de is odd, and de-1 is even.

Good, let de-1 = 2s, then (a^s)^2 = 1 (mod pq). Now a^s is a single root
of 1 mod pq. Let's use 2.8b to try factor pq then.

Important notice: we should iterate over different a such that gcd(a,pq) = 1,
-- there's no guarantee that we will get needed roots for single a.

-}

-- Wants list of (e,d), returns (p,q)
factorizePQ :: Integer -> [(Integer, Integer)] -> (Integer,Integer)
factorizePQ pq eds = (p,pq `div` p)
  where
    hasDistinct xs = length (nub xs) > 1
    as = filter (\x -> gcd x pq == 1) [2..pq]
    solve a = do
        let roots = map (\(e,d) -> exp pq a $ (e * d - 1) `div` 2) eds
        let choices2 = map (\xs -> let [a,b] = take 2 xs in (a,b))
                           (permutations roots)
            sols = concatMap (\(x,y) -> [gcd pq (x - y), gcd pq (x + y)]) choices2
            porq = find (\x -> x /= 1 && pq `div` x /= 1 && x /= pq) sols
        (a,) <$> porq
    (a,p) =
        fromMaybe (error "factorizePQ: can't compute roots") $
        head $ mapMaybe solve as

e310_b = factorizePQ 38749709 [(10988423,16784693),(25910155,11514115)]

e310_d = factorizePQ 1291233941 [(1103927639,76923209),(1022313977,106791263),(387632407,7764043)]

e310_c = factorizePQ 225022969 [(70583995,4911157),(173111957,7346999),(180311381,29597249)]


e310 :: IO ()
e310 = do
    print e310_b
    print e310_c
    print e310_d

{-
λ> e310
(7247,5347)
(10867,20707)
(97151,13291)
-}

----------------------------------------------------------------------------
-- 3.11
----------------------------------------------------------------------------

{-

(a) Since mg_1^s1 = m*g^{s1*r1*(p-1)} = m mod p, same for s2 we clearly get what
we want.
(b) We can easily find p: g1 - 1 = 0 (mod p), so gcd(g1-1, pq) = p.

-}
