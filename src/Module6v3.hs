{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | ECDLP.

module Module6v3 () where

import System.Random
import Universum

import Lib.Elliptic
import Lib.Field
import Lib.Misc (inverse)

----------------------------------------------------------------------------
-- 6.8
----------------------------------------------------------------------------

-- | Solves DLP Q = nP by trial-end-error.
ecdlpTrialAndError :: (FField f, HasECParams f) => EC f -> EC f -> Integer
ecdlpTrialAndError q p =
    fst $
    fromMaybe (error "ecdlpTrialAndError") $
    find ((== q) . snd) $
    map (\z -> (z, z `times` p)) [0..]

e68 :: IO ()
e68 =
    withECParams (ECParams (toZ 1) (toZ 1) :: ECParams (Z 5)) $ do
        let p = EC 4 2
        let q = EC 0 1
        print $ ecdlpTrialAndError @(Z 5) q p
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
You either take P1 = P2 or one of them equal to zero. In second case,
the solution is just ready; in the first case, n1+n2 should be reduced
modulo s.
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

-- binary expansion, little endian
binExpand :: Integer -> [Integer]
binExpand 0 = []
binExpand x = bool 1 0 (even x) : binExpand (x `div` 2)

-- Returns the list of powers (one of {-1, 0, 1}), little endian
ternExpand :: Integer -> [Integer]
ternExpand = shrink . binExpand
  where
    -- returns (i,l) of longest sequence of 1s such that it starts on
    -- ith element and l elems long.
    tryShrink :: [Integer] -> Maybe (Int,Int)
    tryShrink l =
        fmap (first fromIntegral . swap) $
        head $
        reverse $
        sortOn fst $
        filter ((> 2) . fst) $
        map (first $ length . takeWhile (== 1)) $
        tails l `zip` [(0::Integer)..]
    shrink x = case tryShrink x of
      Nothing    -> x
      Just (i,j) ->
          let left []        = [1]
              left (0:xs)    = 1:xs
              left ((-1):xs) = 0:xs
              left imp       = error $ "ternExapnd.left: can't happen: " <> show imp
          in shrink $ take i x <> [-1] <> replicate (j-1) 0 <> left (drop (i+j) x)

testExpand :: IO ()
testExpand = replicateM_ 1000 $ do
    r <- randomRIO @Integer (0,1000000)
    let interpretTern = snd . foldl (\(p,v) i -> (p*2, v + i*p)) (1,0)
    let e = ternExpand r
    let r1 = interpretTern e
    when (r1 /= r) $ error "kek"

ectimes :: forall f. (HasECParams f, FField f) => Integer -> EC f -> EC f
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

Taking 1/j is easy if g = gcd(j,n) = 1. If it's not the case, we will
proceed exactly as we did in the original pollard's algorithm.

We want to find 1/k such that j * 1/k * P = Q
-}

data ECRhoAcc = ECRhoAcc
    { ecrA :: !Integer
    , ecrB :: !Integer
    , ecrC :: !Integer
    , ecrD :: !Integer
    } deriving Show

ecRhoFromAcc :: (HasECParams f, Field f) => ECRhoAcc -> EC f -> EC f -> Integer -> Maybe Integer
ecRhoFromAcc ECRhoAcc{..} p q n =
    case gcd j n of
      1 -> Just $ (k * inverse j n) `mod` n
      -- uj + vn = g
      -- uj = g (mod n)
      g -> let d = n `div` g
               j' = j `mod` d
               k' = k `mod` d
               jinv = inverse j' d
           in
              --traceShow (n,j,g,d,j',gcd j' d) $
              if gcd j' d /= 1
              then Nothing -- I'm not sure what to do.
              else find (\x -> x `times` p == q) $
                   map (\i -> k' + jinv + i) [0..]
  where
    k = (ecrA - ecrC) `mod` n
    j = (ecrD - ecrB) `mod` n

ecRhoGo ::
       forall i. (PrimeNat i, HasECParams (Z i))
    => EC (Z i)
    -> EC (Z i)
    -> Integer
    -> Maybe Integer
ecRhoGo p q k =
    let v = k `times` p
    in go (1 :: Integer) v v (ECRhoAcc k 0 k 0) -- kudah
  where
    go i !x !y !acc
        | x == y && i > 1 = ecRhoFromAcc acc p q n
        | otherwise = do
          --traceShow (i,x,y,acc) $ do
            let (x', a, b) = f (x, ecrA acc, ecrB acc)
            let (y', c, d) = f (f (y, ecrC acc, ecrD acc))
            go (i+1) x' y' (ECRhoAcc a b c d)

    n = ecGroupSize @(Z i)

    inS1 (EC x y) = (x + y) `mod` 3 == 0
    inS1 _        = False
    inS2 (EC x y) = (x + y) `mod` 3 == 1
    inS2 _        = False

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
    go k = maybe (go $ k+1) identity (ecRhoGo p q k)

-- Inverted data from 6.11
e613 :: IO ()
e613 = do
    let solve :: forall m . (PrimeNat m) => Integer -> Integer -> EC (Z m) -> EC (Z m) -> IO ()
        solve a b p q =
            withECParams (ECParams (toZ a) (toZ b) :: ECParams (Z m)) $ do
                --print $ ecGroupSize @(Z m)
                let o = ecOrder p
                print o
                let d = ecRho p q
                print d
                print $ d `mod` o
    putText "#1"
    solve @83 23 13 (EC 24 14) (EC 24 69)
    putText "#2"
    solve @613 143 367 (EC 195 9) (EC 485 573)
    putText "#3"
    solve @1999 1828 1675 (EC 1756 348) (EC 1068 1540)
    putText "#4"
    solve @3221 1541 1335 (EC 2898 439) (EC 243 1875)

{-
Answers are correct, though pollard's method must be optimized.
λ> e613
#1
5
89
4
#2
189
212
23
#3
2058
2069
11
#4
1621
3211
1590
-}
