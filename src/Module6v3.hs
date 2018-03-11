{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | ECDLP.

module Module6v3 () where

import System.Random
import Universum

import Lib.Elliptic
import Lib.Field


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
    let solve :: forall p . (PrimeNat p) => Integer -> Integer -> Integer -> EC (Z p) -> IO ()
        solve a b n q =
            withECParams (ECParams (toZ a) (toZ b) :: ECParams (Z p)) $ do
                print $ n `times` q
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
