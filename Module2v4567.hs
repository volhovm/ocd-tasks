module Module2v4567 () where

import           Control.Monad       (forM_, unless)
import           Data.Bifunctor      (bimap)
import           Data.List           (sortBy)
import           Data.Numbers.Primes (primes)
import           Data.Ord            (comparing)
import           Debug.Trace
import           Prelude             hiding (exp)

import           Lib                 (exp, inverse, logDTrialAndError)

------ 2.8 Elgamal general

-- (a)
e28p = 1373
e28g = 2
e28a = 947
e28A = exp e28p e28g e28a -- 177

elgamalEnc p g pubA k m = (c₁, c₂)
  where
    c₁ = exp p g k
    c₂ = (m * exp p pubA k) `mod` p

elgamalDec p g a (c₁,c₂) = (x * c₂) `mod` p
  where
    x = inverse (exp p c₁ a) p
    pubA = exp p g a

-- (b)
e28b = 716
e28B = exp e28p e28g e28b
e28c = elgamalEnc e28p e28g e28B 877 583 -- (719,623)

-- (c)
e28m = elgamalDec e28p e28g 299 (661, 1325) -- 332

-- (d)

e28Eve = m
  where
    pubB = 893
    c = (693, 793)
    b = logDTrialAndError e28p e28g pubB
    m = elgamalDec e28p e28g b c

------ 2.9 Making use of DH problem to break Elgamal
{-
This is pretty straightforward in fact. If we have function f
that given g^a and g^b calculates g^ab, then we just pass
g^k = c₁ and g^a = A (both public known) to it and thus obtain
g^ka which is c₁^a then inverted to decrypt m.
-}

------ 2.10

-- :(

------ 2.11-2.16 DONE ON PAPER

------ 2.17

shank :: Int -> Int -> Int -> Int
shank p g h = collisionGo list1 list2
  where
    ml a b = (a * b) `mod` p
    getN 1 m  = m
    getN g' m = getN (g' `ml` g) (m+1)
    _N = getN g 1
    n = 1 + floor (sqrt $ fromIntegral _N)
    list1 = sortBy (comparing fst) $ take (n+1) $
        iterate (bimap (ml g) (+1)) (1,0)
    gMinN = exp p g (_N - n) -- g^(-n)
    list2 = sortBy (comparing fst) $ take (n+1) $
        iterate (bimap (ml gMinN) (+1)) (h,0)
    collisionGo [] _ = error "shankErr"
    collisionGo _ [] = error "shankErr"
    collisionGo a@((x,i):xs) b@((y,j):ys) =
        case compare x y of
          EQ -> (i + j * n) `mod` _N
          LT -> collisionGo xs b
          GT -> collisionGo a ys

{-
λ> shank 71 11 21
37
λ> shank 593 156 116
59
λ> shank 3571 650 2213
319
-}

testLogs :: IO ()
testLogs =
    forM_
        [ (p, g, b)
        | p <- take 16 primes
        , g <- [2 .. 100]
        , b <- [1 .. 100]
        , g < p
        , b < p ] $
    \(p, g, b) -> do
        let h = exp p g b
        let label = show p ++ ", " ++ show g ++ ", " ++ show b ++ ", " ++ show h
        putStrLn label
        let b1 = shank p g h
            b2 = logDTrialAndError p g h
        unless (exp p g b1 == h) $ error $ "shank failed: " ++ label
        unless (exp p g b2 == h) $ error $ "trial/error failed: " ++ label
        unless (b1 == b2) $ error $ "logs are not equal: " ++ label
