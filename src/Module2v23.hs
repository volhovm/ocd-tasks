{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Module2v23 () where

import Lib (exp, logD)
import Prelude hiding (exp)

------ 2.3
{-
(a). Let's take n = a - b. Without loss of generality let's assume n ≥
0, otherwise swap a and b.

Then g^(a-b) = g^n. Because g is an element of ℤ/pℤ, then according to
Fermat's little theorem g^(p-1) ≡ 1.

So g^(a-b) ≡ g^(n * (p-1)). Then let's take logarithm from both parts
and get (a - b) = n * (p-1) that exactly means a ≡ b (mod p-1)

Using this statement it's easy to see that every output of log_g is an
element of ℤ/(p-1)ℤ. And every input is just by definition ∈ Fₚ*, that
is the same or even smaller set. Moreover log_g doesn't care about
interpretation of input (log_g(h + np) = log_g(h)), so accordingly to
definition of "well defined functions" from wiki this one is
well-defined.

(b). h₁, h₂ ∈ Fₚ*, so ∃x₁,x₂ such that g^x₁ ≡ h₁ (mod p). Same for x₂.
Thus g^x₁ * g^x₂ ≡ h₁h₂ (mod p), from which the statement directly
follows.

(c). Same n times.
-}

------ 2.4

e24a, e24b, e24c :: Int
e24a = logD 23 2 13
e24b = logD 47 10 22
e24c = logD 941 627 608
{-
λ> e24a
7
λ> e24b
11
λ> e24c
18
-}

------ 2.5
{-
Let's suppose x is discrete log of a base g modulo p. Then:

g^x = a (mod p)

Given that x is even, x = 2 * y, so then:

(g^y)^2 = a (mod p)

That directly states that g^y is the square root of a mod p.
-}

------ 2.6
{-
Because field size is relatively small, Alice's secret exponent can be
brute-forced:
-}
e26_p = 1373
e26_g = 2
e26_a = logD e26_p e26_g e26_A -- 587
e26_A = 974
e26_b = 871 :: Integer
e26_B = exp e26_p e26_g e26_b           -- 805
e26_secretBob = exp e26_p e26_A e26_b   -- 397
e26_secretAlice = exp e26_p e26_B e26_a -- 397
e26_isBullshit = e26_secretBob /= e26_secretAlice -- not bullshit
