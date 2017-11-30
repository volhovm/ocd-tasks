-- | Factorization via difference of primes

module Module3v6 () where

import Universum hiding (exp)
import Unsafe (unsafeHead)

import Control.Lens (at)
import Data.List (findIndex)
import qualified Data.Map as M
import Data.Numbers.Primes (isPrime, primeFactors, primes)

import Lib

----------------------------------------------------------------------------
-- 3.24 Simple difference factorization
----------------------------------------------------------------------------

perfectSquare :: Integer -> Maybe Integer
perfectSquare x =
    let y = truncate $ sqrt (fromIntegral x)
    in if y ^ 2 == x
       then Just y
       else Nothing

e324 :: Integer -> (Integer,Integer)
e324 n = (a-b, a+b)
  where
    (a,b) =
        unsafeHead $
        mapMaybe (\b -> let a2 = n + b^2
                        in (,b) <$> perfectSquare a2) [1..n-1]

{-
λ> e324 53357
(229,233)
λ> e324 34571
(181,191)
λ> e324 25777
(149,173)
λ> e324 64213
(157,409)
-}

----------------------------------------------------------------------------
-- 3.25 Improved difference factorization
----------------------------------------------------------------------------

e325 :: Integer -> Integer -> Integer -> (Integer,Integer)
e325 n k b0 = unsafeHead $ mapMaybe verifyAb abs
  where
    verifyAb (a,b) = do
        let x = a+b
        let y = a-b
        let g1 = gcd n x
        let g2 = gcd n y
        guard $ not $ any (\g -> g == n || g == 1) [g1,g2]
        pure (g1,g2)
    abs = mapMaybe (\b -> let a2 = k * n + b^2
                          in (,b) <$> perfectSquare a2) [b0..n]

{-
λ> e325 143041 247 1
(313,457)
λ> e325 1226987 3 36
(653,1879)
λ> e325 2510839 21 90
(1051,2389)
-}

----------------------------------------------------------------------------
-- 3.26 Even more improved version
----------------------------------------------------------------------------

combinations :: Integer -> [a] -> [[a]]
combinations 0 _  = return []
combinations n xs = do y:xs' <- tails xs
                       ys <- combinations (n-1) xs'
                       return (y:ys)

allCombinations :: [a] -> [[a]]
allCombinations xs = concatMap (flip combinations xs) [1..(toInteger $ length xs)]

-- Given a prime, tells which number is it.
-- λ> pNumber 5
-- 2
-- λ> pNumber 3
-- 1
-- λ> pNumber 2
-- 0
pNumber :: (Integral n, Integral m) => n -> m
pNumber p | not (isPrime $ fromIntegral p) = error "pNumber"
          | otherwise = fromIntegral $ fromMaybe (error "pNumber2") $ findIndex (== p) primes

e326 :: Integer -> [(Integer,[(Integer,Integer)])] -> (Integer,Integer)
e326 n hints =
    (\p -> (p, n `div` p)) $
    unsafeHead $
    mapMaybe (\(a,b) -> let g = gcd (a - b) n
                        in guard (g /= 1 && g /= n) >> pure g)
             combinedHints
  where
    mulMany = foldr1 (\x y -> (x * y) `mod` n)
    combinedHints = do
        let maxHintsP = maximum $ map (\(_,ps) -> maximum $ map fst ps) hints
        let vecLen = pNumber maxHintsP
        let toValidCombination :: [[(Integer,Integer)]] -> Maybe Integer
            toValidCombination [] = Nothing
            toValidCombination (concat -> xs) = do
                let x :: Map Integer Integer
                    x = foldr (\(p,i) m -> m & at p %~ (fmap (+i) . maybe (Just 0) pure)) mempty xs
                guard $ all even $ M.elems x
                pure $ mulMany $ map (\(p,i) -> exp n p (i `div` 2)) $ M.assocs x
        -- Combinations of (a,b).
        let combs :: [(Integer,Integer)]
            combs = mapMaybe (\x -> let y = toValidCombination (map snd x)
                                    in (mulMany (map fst x),) <$> y) $
                    allCombinations hints
        combs

e326Tests :: IO ()
e326Tests = do
    print $ e326 61063 [(1882, [(2,1),(3,3),(5,1)])
                       ,(1898, [(2,1),(3,5),(5,3)])]
    print $ e326 52907 [(399, [(2,5),(3,1),(5,1)])
                       ,(763, [(2,6),(3,1)])
                       ,(773, [(2,6),(3,5)])
                       ,(976, [(2,1),(5,3)])]
    print $ e326 198103 [(1189, [(2,3),(3,3),(5,3)])
                        ,(1605, [(2,1),(7,3)])
                        ,(2378, [(2,5),(3,3),(5,3)])
                        ,(2815, [(3,1),(5,1),(7,1)])]
    print $ e326 2525891 [(1591, [(2,1),(5,1),(7,2),(11,1)])
                         ,(3182, [(2,3),(5,1),(7,2),(11,1)])
                         ,(4773, [(2,1),(3,2),(5,1),(7,2),(11,1)])
                         ,(5275, [(2,3),(3,6),(7,1)])
                         ,(5401, [(2,4),(3,2),(5,3),(7,1),(11,1)])]

{-
λ> e326Tests
(227,269)
(277,191)
(499,397)
(1637,1543)
-}
