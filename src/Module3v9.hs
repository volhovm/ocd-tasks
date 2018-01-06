-- | Quadratic reciprocity

module Module3v9 () where

import Universum hiding (exp)
import Unsafe (unsafeHead)

import Control.Lens (at, ix, uses, (%=), (.=))
import Data.List (last, (!!))
import qualified Data.Map.Strict as M
import Data.Numbers.Primes (isPrime, primeFactors, primes)
import qualified Data.Set as S
import qualified Data.Text as T
import System.Random (randomRIO)

import Lib

----------------------------------------------------------------------------
-- 3.37 Simple congruence problems
----------------------------------------------------------------------------

{-
(a)
g^1...g^{p-1} are all distinct numbers. g^{p-i} is 1. We know that (-1)^2 = 1:
* (p-1)^2 = p^2 -2p + 1 = 1.
* or (-1)^2 = (1 - 2)^2 = (1-2)(1-2) = 1 + 4 - 2 * 2 = 1.

So g^{(p-1)/2} must be -1, because there's no other g's power except for (p-1)/2
that gives (p-1) when doubled.

So 1 can't have more than 2 roots.

Obviously, a^((p-1)/2) = +-1, so both roots are correct and there can be no other.

(b) I'll prove it together with (c)


-- Left-to-right is easy. If c^2 = a, then a^{(p-1)/2} = c^(p-1) = 1, can't be -1.
-- Other way,
-- a^{(p-1)/2} = 1 = g^(p-1)
--   g^{i(p-1)/2} = g^(p-1), so i(p-1)/2

(c)
So let's consider a^{(p-1)/2} to be 1 or -1. a = g^i, let's examine if i is even
or odd.

Let's suppose i is odd, i = 2k+1, then
a^{(p-1)/2}
  = g^{(2k+1)(p-1)/2} =
  = g^{k(p-1) + (p-1)/2}
  = g^{(p-1)/2}
  = -1.

Ok, then let i be even, i = 2k: a^{(p-1)/2} = g^{k(p-1)} = 1

If p | a, then a^{(p-1)/2} = 0^{(p-1)/2} = 0.

(d)
Trivial. Let a = -1, then (-1/p) = (-1)^{(p-1)/2}

It's easy to see that (p-1)/2 is even when p = 4k+1 and
odd if p = 4k+3 (and it's not prime if it's 4k+2 or 4k, obviously).

-}

----------------------------------------------------------------------------
-- 3.38
----------------------------------------------------------------------------

{-
This is highly straightforward.

(a) is proven already.

(b) is obivous. let p = 8k+1, then power of (-1) is 10k
let p = 8k+3, then we get 15k + 1
Same continues...

(g) If p = 4k+1 or q = 4l+1, then power of (-1)
is 2k * X or Y * 2l, which is even and so we prove first clause
of 3.62(c). If both are 4k+3 and 4l+3, then we get odd.
-}

----------------------------------------------------------------------------
-- 3.39
----------------------------------------------------------------------------


sRootP34 :: Integer -> Integer -> Maybe Integer
sRootP34 p a
    | (p `mod` 4 /= 3) = error "sRootP34, p is not 3 mod 4"
    | otherwise = let b = exp p a $ (p + 1) `div` 4
                  in guard (exp p b 2 == a) >> pure b

sRootNaive :: Integer -> Integer -> Maybe Integer
sRootNaive p a = find (\x -> exp p x 2 == a) [1..p-1]

{-
(a) First of all, the proof is already given in 2.26. Anyway,
    b^2 = a^{(p+1)/2} = a * a^{p-1/2} = a * (a/p) = a

(b)
λ> map (uncurry sRootP34) [(587,116),(8627,3217),(10663,9109)]
[Just 65,Just 2980,Nothing]
-}

----------------------------------------------------------------------------
-- 3.40
----------------------------------------------------------------------------

-- | Calculating Jacobi symbol.
isSquareRoot :: Integer -> Integer -> Integer
isSquareRoot = go
  where
    go a b
        | (a `mod` b) == b-1 = case b `mod` 4 of
            1 -> 1
            3 -> -1
        | (a `mod` b) == 2 = case b `mod` 8 of
            1 -> 1
            7 -> 1
            3 -> -1
            5 -> -1
        | even a = go 2 b * go (a `div` 2) b
        | otherwise = case (a `mod` 4, b `mod` 4) of
            (3,3) -> go (-1) a * go (b `mod` a) a
            _     -> go (b `mod` a) a

{-

We'll just proceed as the principle suggests.

So g^{log_g{h}} = h is quadratic residue iff log_g{h} is even.
That's the first bit. How do we get second one? Divide by two,
of course.

Even and odd numbers should be considered differently:

1. If current log is even, we get
   g^{log_g{h}/2} = h^{1/2} is quadratic residue if log_g{h}/2 is even.
2. Otherwise,
   g^{(log_g{h}-1)/2} = sqrt(g^(-1)*h) is what should be tested.

Why will it iterate s steps? No idea for now.

-}
