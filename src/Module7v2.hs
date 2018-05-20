{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Subset-sum problems/knapsack cryptosystems.

module Module7v2 () where

import Universum

import Lib.Misc (allCombinations, inverse)

----------------------------------------------------------------------------
-- 7.2 Solving subset-sum for superincreasing sequences
----------------------------------------------------------------------------

solveSS :: [Integer] -> Integer -> [Bool]
solveSS m s0 =
    snd $
    foldl'
        ( \(s, acc) mi -> if s >= mi then (s - mi, True:acc)
                                     else (s, False:acc))
        (s0, [])
        (reverse m)

hasSolution :: [Integer] -> Integer -> Bool
hasSolution m s = any (\x -> sum x == s) (allCombinations m)

isSS :: [Integer] -> Bool
isSS (x:y:xs) = if 2 * x < y then isSS (y:xs) else False
isSS _        = True

validateSS :: [Integer] -> Integer -> [Bool] -> Bool
validateSS m s b = sum (map (uncurry (*)) $ m `zip` map b2i b) == s
  where b2i False = 0
        b2i True  = 1

e72 :: IO ()
e72 = do
    let solve m s = do
            let x = solveSS m s
            print x
            when (not $ validateSS m s x) $
                putText $ "Invalid. Is superincreasing: " <> show (isSS m) <>
                          ". Has solution: " <> show (hasSolution m s)
    solve [3,7,19,43,89,195] 260
    solve [5,11,25,61,125,261] 408
    solve [2,5,12,28,60,131,257] 334
    solve [4,12,15,36,75,162] 214

{-
λ> e72
[True,False,True,True,False,True]
[True,True,False,False,True,True]
Invalid. Is superincreasing: True. Has solution: False
[False,True,True,False,True,False,True]
[False,False,True,True,False,True]
Invalid. Is superincreasing: False. Has solution: True
-}

----------------------------------------------------------------------------
-- 7.3 Merkle-Hellman Knapsack PKC
----------------------------------------------------------------------------

e73 :: IO ()
e73 = do
    let m = [5186,2779,5955,2307,6599,6771,6296,7306,4115,637]
    let a = 4392
    let b = 8387
    let s = 4398
    let r = map (\x -> (x * inverse a b) `mod` b) m
    let x = solveSS r s
    print r
    print x

{-
λ> e73
[5,14,30,75,160,351,750,1579,3253,6510]
[False,True,True,False,False,True,True,False,True,False]
-}

----------------------------------------------------------------------------
-- 7.4 O(1) storage knapsack collision
----------------------------------------------------------------------------

{-
At each step our function f will switch one bit in M_i -- include or
exclude certain M element. We'll maintain two index lists, both of
length sqrt(n). One sequence is produced by applying f once and
adding/deleting M_i, other -- starting with S and deleting/adding M_i.
Finding a match is then resolved as in proposition 7.3.

TODO Analyse the probability of the false match.
-}
