module Module2v8 () where

import           Data.Bifunctor (bimap)
import           Data.List      (sortBy)
import           Data.Ord       (comparing)
import           Debug.Trace
import           Prelude        hiding (exp)

import           Lib            (inverse, relativePrimes)

------ Let's write chinese algorithm first

-- Accepts pairs of (aᵢ,mᵢ) where x = aᵢ (mod mᵢ).
chinese :: [(Int,Int)] -> Int
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
