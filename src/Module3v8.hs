{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Index calculus method for solving DLP in F_p

module Module3v8 () where

import Universum hiding (exp)
import Unsafe (unsafeLast)

import Data.Numbers.Primes (primeFactors, primes)
import System.Random (randomRIO)

import Lib

mToZ :: forall n. (KnownNat n) => [[Integer]] -> Matrix (Z n)
mToZ = Matrix . map (map $ toZ @n)

testGauss :: IO ()
testGauss = putStrLn $ showMatrix $ gaussSolve m
  where
    m = mToZ @9539 [[2,6,1,3030],[11,2,0,6892],[4,1,3,18312]]

primeFactorsPairs :: Integer -> [(Integer,Integer)]
primeFactorsPairs m =
    let ps = ordNub $ primeFactors m
        fPow n e c = if n `mod` e == 0 then fPow (n `div` e) e (c+1) else c
    in map (\p -> (p, fPow m p 0)) ps

indexCalcDLP :: Integer -> Integer -> Integer -> Integer -> IO Integer
indexCalcDLP p g h b = do
    relations <- replicateM relsN genRelation
    print relations
    gfact <- genGFact
    print gfact
    error "postponed"
  where
    bsmoothEx :: Integer -> Maybe [(Integer,Integer)]
    bsmoothEx x =
        let ps = primeFactorsPairs x
        in bool Nothing (Just ps) $ all ((<= b) . fst) ps

    -- Returns index i and factors (p,ep) if g^i is b-smooth.
    genRelation :: IO (Integer, [(Integer,Integer)])
    genRelation = do
        i <- randomRIO (2, p-1)
        maybe genRelation (pure . (i,)) $ bsmoothEx $ exp p g i

    -- Returns k and factorization for h * g^{-k}
    genGFact :: IO (Integer, [(Integer,Integer)])
    genGFact = do
        k <- randomRIO (2, p-1)
        maybe genGFact (pure . (k,)) $ bsmoothEx $ (h * exp p g (- k)) `mod` p

    relsN = length primeBase
    primeBase = takeWhile (<= b) primes

e336 :: IO ()
e336 = do
    let p = 19079
    let g = 17
    let b = 5
    let ixs = [3030, 6892, 18312]
    let k = 12400
    -- (a)
    print $ all (\i -> bsmooth b $ exp p g i) ixs
    let m = [[2,6,1,3030],[11,2,0,6892],[4,1,3,18312]]
    let sols1 = map unZ $ unsafeLast $ unMatrix $ gaussSolve $ mToZ @9539 m
    let sols2 = map unZ $ unsafeLast $ unMatrix $ gaussSolve $ mToZ @2 m
    print sols1
    print sols2
    -- [8195,1299,7463]
    -- [0,0,0]
    let solsrly = map (\(e1,e2) -> crt [(e1,9539),(e2,2)]) $ sols1 `zip` sols2
    -- [17734,10838,17002]
    -- (b)
    print solsrly
    -- (c)
    print $ bsmooth b $ (19 * exp p g (- k)) `mod` p
    -- (d)
    print $ primeFactorsPairs $ (19 * exp p g (- k)) `mod` p
    print $ (k + 17734 * 7 + 10838) `mod` (p - 1)
    -- 13830
