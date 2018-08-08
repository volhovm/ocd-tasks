{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | ECDLP.

module Module6v3
       ( ecRho
       , binExpand
       , ternExpand
       ) where

import Universum

import Data.List ((!!))
import System.Random

import Lib.Elliptic
import Lib.Field
import Lib.Misc (binExpand, exEucl, inverse, ternExpand)

----------------------------------------------------------------------------
-- 6.8
----------------------------------------------------------------------------

-- | Solves DLP nP = Q by trial-end-error.
ecdlpTrialAndError :: (FField f, HasECParams f) => EC f -> EC f -> Integer
ecdlpTrialAndError p q =
    fst $
    fromMaybe (error "ecdlpTrialAndError") $
    find ((== q) . snd) $
    map (\z -> (z, z `times` p)) [0..]

e68 :: IO ()
e68 =
    withECParams (ECParams (toZ 1) (toZ 1) :: ECParams (Z 5)) $ do
        let p = EC 4 2
        let q = EC 0 1
        print $ ecdlpTrialAndError @(Z 5) p q
        -- It's 5

----------------------------------------------------------------------------
-- 6.9
----------------------------------------------------------------------------

{-
Q = nP = (is + r)P = (is)P + rP = rP

The only non-trivial step is (a+b)P = aP + bP, but this too directly
follows from scalar multiplication definition.
-}

----------------------------------------------------------------------------
-- 6.10
----------------------------------------------------------------------------

{-
Let's assume we can solve the basis problem for {P1, P2}.

Applied to kP = Q, P = n1P1 + n2P2, Q = m1P1 + m2P2:

k(n1P1 + n2P2) = m1P1 + m2P2
(m1 - k*n1) P1 + (m2 - k*n2) P2 = 0

Since P1 and P2 are basis, their coefficients must be zero, otherwise
they're linearly dependent.

So just solve m1 - k*n1 = 0 in the same way it's done in pollard.
-}

----------------------------------------------------------------------------
-- 6.11
----------------------------------------------------------------------------

-- `times` is already implemented using double-and-add.

e611 :: IO ()
e611 = do
    let solve :: forall m . (PrimeNat m) => Integer -> Integer -> Integer -> EC (Z m) -> IO ()
        solve a b n p =
            withECParams (ECParams (toZ a) (toZ b) :: ECParams (Z m)) $ do
                print $ n `times` p
    solve @83 23 13 19 (EC 24 14)
    solve @613 143 367 23 (EC 195 9)
    solve @1999 1828 1675 11 (EC 1756 348)
    solve @3221 1541 1335 3211 (EC 2898 439)

{-
λ> e611
EC 24 69
EC 485 573
EC 1068 1540
EC 243 1875
-}

----------------------------------------------------------------------------
-- 6.12
----------------------------------------------------------------------------

testExpand :: IO ()
testExpand = replicateM_ 1000 $ do
    r <- randomRIO @Integer (0,1000000)
    let interpretTern = snd . foldl (\(p,v) i -> (p*2, v + i*p)) (1,0)
    let e = ternExpand r
    let r1 = interpretTern e
    when (r1 /= r) $ error "kek"


ectimes :: forall f. (HasECParams f, Field f) => Integer -> EC f -> EC f
ectimes n0 p0 =
    snd $ foldl (\(p,v) i -> (p<+>p, case i of
                                       0    -> v
                                       1    -> v <+> p
                                       (-1) -> v <-> p
                                       _    -> error "ectimes"
                             ))
                (p0,f0)
                (ternExpand n0)

e612 :: IO ()
e612 = do
    let solve :: Integer -> IO ()
        solve x =
             putText $ show (length $ filter (/= 0) (binExpand x)) <>
                       " vs " <> show (length $ filter (/= 0) (ternExpand x))
    solve 349
    solve 9337
    solve 38728
    solve 8379483273489

{-
6 vs 5
7 vs 5
7 vs 6
21 vs 11
-}

----------------------------------------------------------------------------
-- 6.13
----------------------------------------------------------------------------

{-
In F_p we had equation of type g^a*h^b = g^c*h^d. To come up with
something similar here, let's try to mimic pollard's pseudorandom
function. Let's say we will either double value, add P or Q to it. Let
the starting point be P, because we don't have 1. So then:

x_i = (f∘f∘...∘f)(P) = nP + mQ.

Seems right. In the end we'll have aP + bQ = cP + dQ, Let k = a-c, j =
d-b. Then, kP = jQ, and (k/j)P = Q. 1/j is j's multiplicative inverse
in field F_n, where n is a size of E(F_p).

Taking 1/j is easy if d = gcd(j,n) = 1. If it's not the case, we will
proceed exactly as we did in the original pollard's algorithm.

kP = jQ, so j * log_P(Q) = k (mod n), where n is order of the group

uj = d (mod n)

d * log_P(Q) = ku (mod n)
log_P(Q) = ku/d (mod n)
then k*u/d + i(n/d) is a set of soltuions, i ∈ 0..d-1

Important notice: it only works if P is generator. This limitation is
easy to overcome, but it's not done here.
-}

data ECRhoAcc = ECRhoAcc
    { ecrA :: !Integer
    , ecrB :: !Integer
    , ecrC :: !Integer
    , ecrD :: !Integer
    } deriving Show

ecRhoFromAcc :: (HasECParams f, Field f,Show f) => ECRhoAcc -> EC f -> EC f -> Integer -> Integer
ecRhoFromAcc ECRhoAcc{..} p q n
    | k `times` p /= j `times` q = error "ecRhoFromAcc: wrong args"
    | otherwise =
     case exEucl j n of
      (1,_,_) -> (k * inverse j n) `mod` n
      (d,((`mod` n) -> u),_) ->
          let n' = n `div` d
              w = (k * u) `mod` n
          in fromMaybe (error "ecRhoFromAcc: P is not a generator") $
             head $
             filter (\x -> x `times` p == q) [w `div` d + i * n' | i <- [0..d-1]]
  where
    k = (ecrA - ecrC) `mod` n
    j = (ecrD - ecrB) `mod` n

ecRhoGo ::
       forall i. (PrimeNat i, HasECParams (Z i))
    => EC (Z i)
    -> EC (Z i)
    -> Integer
    -> Integer
ecRhoGo p q k =
    let v = k `times` p
    in go (1 :: Integer) v v (ECRhoAcc k 0 k 0) -- kudah
  where
    go i !x !y !acc
        | x == y && i > 1 = ecRhoFromAcc acc p q n
        | otherwise = do
--            traceShow (i,x,y,acc) $ do
            let (x', a, b) = f (x, ecrA acc, ecrB acc)
            let (y', c, d) = f (f (y, ecrC acc, ecrD acc))
            go (i+1) x' y' (ECRhoAcc a b c d)

    n = ecGroupSize @(Z i)

    inS1 (EC (Z x) (Z y)) = (x + y) `mod` 3 == 0
    inS1 _                = False
    inS2 (EC (Z x) (Z y)) = (x + y) `mod` 3 == 1
    inS2 _                = False

{-
    m :: Z i
    m = toZ $ getOrder @(Z i)
    inS1 (EC x _) = x < (m `div` 3)
    inS1 _        = False
    inS2 (EC x _) = x < ((m `div` 3) * 2)
    inS2 _        = False
-}

    f :: (EC (Z i), Integer, Integer) -> (EC (Z i), Integer, Integer)
    f (e, a, b)
        | inS1 e = (e <+> p, (a+1) `mod` n, b)
        | inS2 e = (e <+> e, (a*2) `mod` n, (b*2) `mod` n)
        | otherwise = (e <+> q, a, (b+1) `mod` n)

ecRho ::
       forall i. (PrimeNat i, HasECParams (Z i))
    => EC (Z i)
    -> EC (Z i)
    -> Integer
ecRho p q = let n = go 1 in if n `times` p == q then n else error "ecRho"
  where
    go k = ecRhoGo p q k

-- Inverted data from 6.11
e613 :: IO ()
e613 = do
    let test :: forall m . (PrimeNat m) => Integer -> Integer -> IO ()
        test a b =
            withECParams (ECParams (toZ a) (toZ b) :: ECParams (Z m)) $ do
                let groupSize = ecGroupSize @(Z m)
                let isGen (x :: EC (Z m)) = ecOrder x == groupSize
                let generators = filter isGen listAllPoints
                replicateM_ 10 $ do
                    !p <- (cycle generators !!) <$> randomRIO (5,15)
                    n <- randomRIO (1,groupSize)
                    let q = n `times` p
                    unless (ecOrder p == ecGroupSize @(Z m)) $ error "e613: P is not gen"
                    let sol = ecRho p q
                    unless (sol `times` p == q) $ error "e613 failed"
    putText "#1"
    test @83 23 13
    putText "#2"
    test @613 143 367
    putText "#3"
    test @1999 1828 1675
    putText "#4"
    test @3221 1541 1335
