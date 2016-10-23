{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString     as BS
import           Data.Numbers.Primes (isPrime, primeFactors, primes)
import           Data.Word           (Word8)
import           Prelude

------ 1.42
badday :: [Word8]
badday = BS.unpack "Bad day, Dad."

------ 1.43 (a)

inverse :: Int -> Int -> Int
inverse a p = a `power` (p-2)
  where
    power a 0 = 0
    power a 1 = a `mod` p
    power a b = ((power a (b-1)) `mod` p) * a `mod` p


encryptAffine :: (Int, Int) -> Int -> Int -> Int
encryptAffine (k1, k2) p m = ((k1 * m) `mod` p + k2) `mod` p

decryptAffine :: (Int, Int) -> Int -> Int -> Int
decryptAffine (k1, k2) p c = (k1' * ((c - k2) `mod` p)) `mod` p
  where
    k1' = inverse k1 p

{-
λ> encryptAffine (34, 71) 541 204
515
λ> decryptAffine (34, 71) 541 515
204
-}

------ 1.43 (b)
{-
Exactly two. Assume that (all mod p):
cᵢ = k₁mᵢ + k₂
cⱼ = k₁mⱼ + k₂

Then we can move k₂ to the left, divide equations and uniquely find k₂
from that:

k₂ = (cᵢmⱼ - cⱼmᵢ) / (mⱼ-mᵢ)

Summing two equations we can get another one, which will give us k₁
when k₂ is substituted.

k₁ = (cᵢ + cⱼ - 2k₂) / (mᵢ + mⱼ)

-}

------ 1.43 (c)

{-
p = 601
c₁ = 324
c₂ = 381
m₁ = 387
m₂ = 491
m₃ = 173

k₂ = (324 * 491 - 381 * 387) * (491 - 387)⁻¹
k₁ = (324 + 381 - 2 * k₂) * (mᵢ + mⱼ)⁻¹

k₁ = 41, k₂ = 83

c₃ = 565
-}

e143_k1, e143_k2, e143_c3 :: Int
e143_k2 = (((((324 * 491) `mod` p) - ((381 * 387) `mod` p)) `mod` p) *
      (inverse ((491 - 387) `mod` p) p)) `mod` p
  where
    p = 601
e143_k1 = (((324 + 381 - 2 * e143_k2) `mod` p) *
           (inverse ((387 + 491) `mod` p) p)) `mod` p
  where
    p = 601
e143_c3 = encryptAffine (e143_k1, e143_k2) 601 173

------ 1.43 (d)
{-
In general form, i see a problem like solving this equation system:

c₁ - k₁m₁ - k₂ = n₁p
c₂ - k₁m₂ - k₂ = n₂p
c₃ - k₁m₃ - k₂ = n₃p
...
cᵢ - k₁mᵢ - k₂ = nᵢp

Number of unknown variables always is always 3 more than the number of
equations, so the system is not solvable in general. Though i suppose
if i is relatively big, we can assume that there are collisions among
{nᵢ}ᵢ set. To solve, we need to decrease the number of variables by 3,
so we can iterate over all combination of size < 3 (1 * C³ᵢ or 2 * C²ᵢ
will give us needed number of vars).
-}

------ 1.44 Hill cipher
{-
I'll mostly skip this one because it's similar to 1.43.

a) Inverse matrix is calculated in the same manner as ususal, but all
operations are taken modulo p.

b) First glance shows that there's no difference between usual and
matrix arithmetics, so solution from 1.43 will work.

c) --

d) When K₁ is diagonal matrix with element K₁(i,i) = k₁ᵢ, (i ≠ j ⇒
K₁(i,j) = 0) and K₂ is vector where i'th element is k₂ᵢ, then for every
character from initial alphabet mᵢ ∈ A (|A| = p, i is the position in
M), it will be transformed:

nᵢ = mᵢ * k₁ᵢ + k₂ᵢ (mod p)

so we can represent ceasar's cypher choosing K₁ = I and substitution
cipher in general.
-}

------ 1.45 Is encryption function?

{-
N is large, N ∈ ℕ.

1) eₖ(m) = k - m (mod N); dₖ(c) = k - c (mod N)

Too simple, but i can't understand what's the crux. Seems like a
normal encryption function if N is relatively big :(

c₁ = k - m₁
c₂ = k - m₂

All we can find out is m₂ - m₁ = c₁ - c₂ (mod N).

2) eₖ(m) = km (mod N); That's not an encryption function, because if k
is not unit of ℤₚ, we won't be able to compute k⁻¹. For keys that are
units it will work.

3) eₖ(m) = (k + m)² (mod N)

It doesn't seem like that's a real encryption function.  At least if k
= 2, N = 10, then for m = 0 and m = 6 cypher c will be the same (4),
so it's fucked up a little bit.

-}

------ 1.46 skipped (xor shit)

------ 1.47 Key space
{-
|K| = 2⁵⁶.
v = 10^10 keys per second.

a) 2^55 / 10^10 = 3.6 * 10^6 = 41 day
b) 100 years * 10^10 = 3.15 * 10^19
   2^65 = 3.68 * 10^19
   So half of keys in the keyspace of size 2^66 can be checked in slightly more than 100 years.
-}

------ 1.48 Xor

{-
a) Obviously, if both m and c are known, it's easy to compute k.
kᵢ = if cᵢ then ¬mᵢ else mᵢ (more precisely kᵢ = ¬(mᵢ → cᵢ)).
b) neeh, dolgo
-}


------ 1.49

getAlpha :: Int -> Int -> Int
getAlpha d k = fromInteger $ floor $ (α₀ - (cast $ floor α₀)) * (10 ** (cast d))
  where
    cast = fromInteger . toInteger
    α₀ :: Double
    α₀ = sqrt $ cast k

encryptAlpha :: Int -> Int -> Int -> Int
encryptAlpha d k m = (m + (getAlpha d k)) `mod` (10 ^ d)

decryptAlpha :: Int -> Int -> Int -> Int
decryptAlpha d k c = (c - (getAlpha d k)) `mod` (10 ^ d)

-- 645597
e149_a :: Int
e149_a = encryptAlpha 6 11 328973

-- 8600751
e149_b :: Int
e149_b = decryptAlpha 8 23 78183903

-- (c) is obvious

-- (d) ... lazy

------ 1.50

e150_gcd = gcd 12849217045006222 6485880443666222

factors = primeFactors e150_gcd -- result [2,87192883], so k = 87192883


----------------
main :: IO ()
main = undefined
