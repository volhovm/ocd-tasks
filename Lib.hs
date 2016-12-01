{-# LANGUAGE BangPatterns #-}
-- | Some useful common functions extracted from tasks. I first solve
-- them in per-section-modules, then transfer here and use in next
-- sections. Not for real cryptographic use ofc.

module Lib
       ( exp
       , relativePrimes
       , inverse
       , inverseP
       , order
       , logD
       , logDTrialAndError
       , logDShank
       , crt
       ) where

import           Control.Exception.Base (assert)
import           Control.Monad          (forM_, when)
import           Data.Bifunctor         (bimap)
import           Data.List              (sortBy)
import           Data.Maybe             (fromMaybe, isNothing)
import           Data.Ord               (comparing)
import           Debug.Trace
import           Prelude                hiding (exp)

-- | Log exponential
exp :: (Integral n) => n -> n -> n -> n
exp _ g 0 = 0
exp p g 1 = g `mod` p
exp p g n = case n `mod` 2 of
    0 -> (subexp * subexp) `mod` p
    1 -> (subexp * subexp * g) `mod` p
  where
    !subexp = exp p g $ n `div` 2

-- | Checks if every two elements of the list have gcd = 1
relativePrimes :: (Integral n) => [n] -> Bool
relativePrimes l =
    all ((== 1) . uncurry gcd) $ [(a,b) | a <- l, b <- l, a < b]

-- | Multiplicative inverse modulo p using fermat's little
-- theorem. Here's something about comparing fermat's little theorem
-- approach and euclid's algorithm (implemented in this repo too):
-- http://www.ams.org/journals/mcom/1969-23-105/S0025-5718-1969-0242345-5/S0025-5718-1969-0242345-5.pdf
inverseP :: (Integral n) => n -> n -> n
inverseP a p = a `power` (p-2)
  where
    power a 0 = 0
    power a 1 = a `mod` p
    power a b = ((power a (b-1)) `mod` p) * a `mod` p

-- | Inverse for non-primes
inverse :: (Integral n) => n -> n -> n
inverse a n = head $ filter (\x -> x * a `mod` n == 1) [0..n-1]

logD :: (Integral n) => n -> n -> n -> n
logD p g h = let ans = logDTrialAndError p g h
             in assert (exp p g ans == h) $
                assert (ans < p) $
                ans

-- FIXME ineffective, do lagrange instead
order :: (Integral n) => n -> n -> Maybe n
order p g | g >= p = order p $ g `mod` p
order p g = case filter (\e -> exp p g e == 1) $ factors (p-1) of
    []    -> Nothing
    (k:_) -> Just k
  where
    divides m n = n `mod` m == 0
    factors n = n : [x | x <- [1..n`div`2], x `divides` n]

-- | Trial-and-error discrete logarithm solving algorithm
logDTrialAndError :: (Integral n) => n -> n -> n -> n
logDTrialAndError p g h =
    fst . head $ filter ((== h) . snd) $
    map (\x -> (x, exp p g x)) (reverse [0..p-1])

-- | Solving log problem with shank algorithm. Requires g to be in set
-- of field units.
logDShank :: (Integral n) => n -> n -> n -> n
logDShank p g h
    | g == h = 1
    | isNothing _N0 = logDTrialAndError p g h
    | h == 1 = _N -- FIXME O(n)! Doesn't work on (3,1,2)
    | otherwise = collisionGo list1 list2
  where
    ml a b = (a * b) `mod` p
    getN 1 m  = m
    getN g' m = getN (g' `ml` g) (m + 1)
    _N0 = order p g
    _N = fromMaybe (error "shank called with bad g") _N0
    n = 1 + floor (sqrt $ fromIntegral _N)
    list1 =
        sortBy (comparing fst) $
        take (fromIntegral $ n + 1) $ iterate (bimap (ml g) (+ 1)) (1, 0)
    gMinN = exp p g (_N - n) -- g^(-n)
    list2 =
        sortBy (comparing fst) $
        take (fromIntegral $ n + 1) $ iterate (bimap (ml gMinN) (+ 1)) (h, 0)
    collisionGo [] _ = error "shankErr"
    collisionGo _ [] = error "shankErr"
    collisionGo a@((x, i):xs) b@((y, j):ys) =
        case compare x y of
            EQ -> (i + j * n) `mod` _N
            LT -> collisionGo xs b
            GT -> collisionGo a ys

-- TODO returns trivial solutions if exist. is it correct?
-- | Chinese remainder theorem, accepts list of (a_i,m_i) where x =
-- a_i (mod mi) is a pattern for equations.
crt :: [(Integer,Integer)] -> Integer
crt [] = error "chinese called with empty list"
crt xs | not (relativePrimes $ map snd xs) =
             error $ "not relative primes: " ++ show (map snd xs)
crt ((a₁,m₁):xs) = chineseGo xs (a₁ `mod` m₁) m₁
  where
    chineseGo [] c _              = c
    chineseGo x@((a, m):xs) c mprod =
        chineseGo xs c' (mprod * m)
        where
          m' = inverse mprod m
          y = (m' * ((a - c) `mod` m)) `mod` m
          c' = c + mprod * y
