-- | Some useful common functions extracted from tasks. I first solve
-- them in per-section-modules, then transfer here and use in next
-- sections.

module Lib
       ( exp
       , relativePrimes
       , inverse
       , logD
       , logDTrialAndError
       , logDShank
       ) where

import           Data.Bifunctor (bimap)
import           Data.List      (sortBy)
import           Data.Ord       (comparing)
import           Prelude        hiding (exp)

-- | Stupid exponentation modulo p
exp :: (Integral n) => n -> n -> n -> n
exp _ _ 0 = 1
exp p g n = (g * (exp p g $ pred n)) `mod` p

-- | Checks if every two elements of the list have gcd = 1
relativePrimes :: (Integral n) => [n] -> Bool
relativePrimes l =
    all ((== 1) . uncurry gcd) $ [(a,b) | a <- l, b <- l, a < b]

-- | Multiplicative inverse modulo p using fermat's little
-- theorem. Here's something about comparing fermat's little theorem
-- approach and euclid's algorithm (implemented in this repo too):
-- http://www.ams.org/journals/mcom/1969-23-105/S0025-5718-1969-0242345-5/S0025-5718-1969-0242345-5.pdf
inverse :: (Integral n) => n -> n -> n
inverse a p = a `power` (p-2)
  where
    power a 0 = 0
    power a 1 = a `mod` p
    power a b = ((power a (b-1)) `mod` p) * a `mod` p

logD :: (Integral n) => n -> n -> n -> n
logD = logDTrialAndError

-- | Trial-and-error discrete logarithm solving algorithm
logDTrialAndError :: (Integral n) => n -> n -> n -> n
logDTrialAndError p g h =
    fst . head $ filter ((== h) . snd) $
    map (\x -> (x, exp p g x)) [1..p-1]

-- | Solving log problem with shank algorithm
logDShank :: (Integral n) => n -> n -> n -> n
logDShank p g h = collisionGo list1 list2
  where
    ml a b = (a * b) `mod` p
    -- TODO getN is O(p), should be O(sqrt(p))
    getN 1 m  = m
    getN g' m = getN (g' `ml` g) (m+1)
    _N = getN g 1
    n = 1 + floor (sqrt $ fromIntegral _N)
    list1 = sortBy (comparing fst) $ take (fromIntegral $ n+1) $
        iterate (bimap (ml g) (+1)) (1,0)
    gMinN = exp p g (_N - n) -- g^(-n)
    list2 = sortBy (comparing fst) $ take (fromIntegral $ n+1) $
        iterate (bimap (ml gMinN) (+1)) (h,0)
    collisionGo [] _ = error "shankErr"
    collisionGo _ [] = error "shankErr"
    collisionGo a@((x,i):xs) b@((y,j):ys) =
        case compare x y of
          EQ -> (i + j * n) `mod` p
          LT -> collisionGo xs b
          GT -> collisionGo a ys
