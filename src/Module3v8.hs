-- | Index calculus method for solving DLP in F_p

module Module3v8 () where

import Universum hiding (exp)
import Unsafe (unsafeHead)

import Control.Lens (ix, uses, (%=), (.=))
import Data.List (last, (!!))
import qualified Data.Map.Strict as M
import Data.Numbers.Primes (isPrime, primeFactors, primes)
import qualified Data.Set as S
import qualified Data.Text as T
import System.Random (randomRIO)

import Lib

-- | Matrix is row-dominated.
showMatrix :: [[Integer]] -> Text
showMatrix m = T.unlines $ map (\line -> T.intercalate " " $ map show line) m

-- | You pass linear system [A|b], where A is n×n and get list of
-- solutions.
gaussSolve :: (MonadIO m) => Integer -> [[Integer]] -> m [[Integer]]
gaussSolve p matrix = do
    let m1 = initialSort matrix
    putText $ showMatrix m1
    pure $ execState (diagonal1 >> diagonal2) m1
  where
    ix2 :: Int -> Int -> State [[Integer]] Integer
    ix2 i j = do (x :: [Integer]) <- use (ix i)
                 pure $ x !! j

    n = length matrix
    m = length $ unsafeHead matrix

    sub a b = (a - b) `mod` p
    mul a b = (a * b) `mod` p
    add a b = (a + b) `mod` p

    diagonal1 :: State [[Integer]] ()
    diagonal1 = forM_ [0..(n-1)] $ \(i::Int) -> do
        -- Dividing by diagonal coefficient
        k <- ix2 i i
        let km1 = inverseP k p
        forM_ [i..(m-1)] $ \j -> (ix i . ix j) %= (`mul` km1)

        -- For all lower levels, adding
        forM_ [i+1..(n-1)] $ \j -> do
            s <- ix2 j i
            forM_ [i..m] $ \y -> do
                x <- ix2 i y
                ix j . ix y %= (\e -> e `sub` (s `mul` x))

    diagonal2 :: State [[Integer]] ()
    diagonal2 = forM_ (reverse [0..(n-1)]) $ \(i::Int) -> do
        -- For all upper levels, adding
        forM_ [0..i-1] $ \j -> do
            s <- ix2 j i
            forM_ [i..(m-1)] $ \y -> do
                x <- ix2 i y
                ix j . ix y %= (\e -> e `sub` (s `mul` x))


    initialSort = sortBy (comparing $ length . takeWhile (== 0))

testGauss :: IO ()
testGauss = putText . showMatrix =<< gaussSolve 9539 m
  where
    m = [[2,6,1,3030],[11,2,0,6892],[4,1,3,18312]]

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
    let primeBase = takeWhile (<= b) primes
    let ixs = [3030, 6892, 18312]
    let k = 12400
    -- (a)
    print $ all (\i -> bsmooth b $ exp p g i) ixs
    let curLog = logD p g
    let m = [[2,6,1,3030],[11,2,0,6892],[4,1,3,18312]]
    sols1 <- map last <$> gaussSolve 9539 m
    sols2 <- map last <$> gaussSolve 2 m
    print sols1
    print sols2
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
