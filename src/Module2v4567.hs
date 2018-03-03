{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Module2v4567 () where

import Prelude hiding (exp)

import Control.Monad (forM_, unless)
import Data.Numbers.Primes (primes)

import Lib (exp, inverse, logDShank, logDTrialAndError)

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

elgamalDec p a (c₁,c₂) = (x * c₂) `mod` p
  where
    x = inverse (exp p c₁ a) p

-- (b)
e28b = 716 :: Integer
e28B = exp e28p e28g e28b
e28c = elgamalEnc e28p e28g e28B 877 583 -- (719,623)

-- (c)
e28m = elgamalDec e28p 299 (661, 1325) -- 332

-- (d)

e28Eve = m
  where
    pubB = 893
    c = (693, 793)
    b = logDTrialAndError e28p e28g pubB
    m = elgamalDec e28p b c

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
        , g <- [(2::Int) .. 100]
        , b <- [(1::Int) .. 100]
        , g < p
        , b < p ] $
    \(p, g, b) -> do
        let h = exp p g b
        let label = show g ++ "^" ++ show b ++ " = " ++ show h ++ " mod " ++ show p
        -- putStrLn label
        let b1 = logDShank p g h
            b2 = logDTrialAndError p g h
        unless (exp p g b1 == h) $ error $ "shank failed: " ++ label
        unless (exp p g b2 == h) $ error $ "trial/error failed: " ++ label
        -- The situation where b1 /= b2 is completely normal, since noone guarantees
        -- that g is group generator.
        -- unless (b1 == b2) $
        --     putStrLn $
        --     "logs are not equal: " ++ label ++ ": " ++ show b1 ++ ", " ++ show b2
