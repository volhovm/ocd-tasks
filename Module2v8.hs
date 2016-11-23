module Module2v8 () where

import           Control.Monad       (guard)
import           Data.Bifunctor      (bimap)
import           Data.List           (sortBy)
import           Data.Maybe          (catMaybes, isJust)
import           Data.Numbers.Primes (primeFactors)
import           Data.Ord            (comparing)
import           Debug.Trace
import           Prelude             hiding (exp)

import           Lib                 (exp, inverse, relativePrimes)

------ Let's write chinese algorithm first

-- Accepts pairs of (aᵢ,mᵢ) where x = aᵢ (mod mᵢ).
chinese :: [(Integer,Integer)] -> Integer
chinese [] = error "chinese called with empty list"
chinese xs | not (relativePrimes $ map snd xs) = error "not relative primes"
chinese ((a₁,m₁):xs) = chineseGo xs (a₁ `mod` m₁) m₁
  where
    chineseGo [] c _              = c
    chineseGo ((a, m):xs) c mprod =
        let m' = inverse mprod m
            y = (m' * ((a - c) `mod` m)) `mod` m
            c' = c + mprod * y
        in chineseGo xs c' (mprod * m)

------ 2.18

e218 = do
    print $ chinese [(3,7), (4,9)]
    print $ chinese [(137,423), (87,191)]
    print $ chinese [(133,451), (87,237)]
    print $ chinese [(5,9), (6,10), (7,11)]
    print $ chinese [(37,43), (22,49), (18,71)]

{-
λ> e218
52
27209
49743
194
139959
-}


------ 2.19

-- 23
e219 = chinese [(2,3), (3,5), (2,7)]


------ 2.20 on the paper
------ 2.21 on the paper
------ 2.22 neosilil

------ 2.23 Square roots

-- Should be called with primes only
sqrtP :: Integer -> Integer -> Maybe Integer
sqrtP p a0 | p `mod` 4 /= 3 =
             case take 1 $ filter (\x -> exp p x 2 == a0 `mod` p) [0..p-1] of
               []  -> Nothing
               [a] -> Just a
sqrtP p a0 = do
    --guard $ p + 1 `mod` 4 == 0
--    traceShowM a
--    traceShowM b
    guard $ exp p b 2 == a
    pure b
  where
    a = a0 `mod` p
    b = exp p a $ (p + 1) `div` 4

sqrtPN :: Integer -> Integer -> Maybe [Integer]
sqrtPN n a = do
    traceShowM ps
    traceShowM squares
    traceShowM permutations
    traceShowM chineseInput
    traceShowM chineseSolved
    guard $ all isJust squares
    pure chineseSolved
  where
    ps = primeFactors n
    squares = map (\p -> sqrtP p $ a `mod` p) ps
    permutations = perms $ catMaybes squares
    perms xs = perms' [[]] $ reverse xs
    perms' ys []     = ys
    perms' ys (x:xs) = perms' (map (x :) ys ++ map ((-x) :) ys) xs
    chineseInput = map (\xs -> map (\(a, m) -> (a `mod` m, m)) $ xs `zip` ps) permutations
    chineseSolved = map chinese chineseInput

{-
λ> sqrtPN 437 340
Just [215,291,146,222]
λ> sqrtPN 3143 253
Just [1387,2654,489,1756]
λ> sqrtPN 4189 2833
Just [1712,3187,1002,2477]
λ> sqrtPN 868 813
Just [785,393,41,517,785,393,41,517,785,393,41,517,785,393,41,517]
λ> nub <$> sqrtPN 868 813
Just [785,393,41,517]
-- well, only four -- this can be explained
λ> primeFactors 868
[2,2,7,31]
λ> 813 `mod` 2
1
λ> 813 `mod` 7
1
So we have like only 2 distinct square roots --
  1 and 10 (exp 31 10 2 == 813 `mod` 31 == 7)

-}

------ 2.24 :|

-- returns square root of a mod p^2 knowing b -- sqrt a mod p
-- but not really
e224b p a b =
    traceShow m $
    traceShow check $
    res
  where
    p2 = p * p
    mpp = (- m * p2) `mod` p2
    res = exp p2 (b + mpp) 2
    check = exp (p * p) res 2 == a `mod` p2
    m = (b ^ 2) `div` p

------ 2.25 on paper
