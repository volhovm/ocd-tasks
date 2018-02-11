{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Probability theory basics, again and again (just in
-- case). Something is skipped because it's trivial.

module Module5v3 () where

import Universum

----------------------------------------------------------------------------
-- 5.22
----------------------------------------------------------------------------

{-
Probability of Alice's win is (14 choose 7)/2^14 = 0.2095, so
expectation of my balance is 0.2095*4 + (1-0.2095)*(-1) = 4.75 cents.

Repeated ten thousand times, i win 475 dollars.

-}

----------------------------------------------------------------------------
-- 5.23
----------------------------------------------------------------------------

{-
(a) p(E|E) = p(E&E) / p(E) = 1. Because if E happened, E must have happened.
(b) p(E|F) = p(E&F) / smth = 0.
(c), (d), (e) Also trivial and boring.
-}

----------------------------------------------------------------------------
-- 5.24
----------------------------------------------------------------------------

{-
A = pen, B = pencil, U_i = urn#i.

(a) P(B) = 1/2 * (7/10 + 4/12) = 31/60 = 0.516
(b) p(U1|B) = P(A|U1)*P(U1)/P(B) = (7/10) * (1/2) / (31/60) = 21/31 = 0.678
(c) p(B)^2 = 0.267
-}

----------------------------------------------------------------------------
-- 5.25
----------------------------------------------------------------------------

{-
(a) 1/3
(b) p(sssssg) = (2/3)^5 * 1/3 = 0.0439
-}

----------------------------------------------------------------------------
-- 5.26 Three prisoners problem
----------------------------------------------------------------------------

{-
(a) P(B|J) = 0, P(J|B) = 0, P(J|C) = P(JC)/P(C) = (1/3)/(1/3) = 1
(b) P(J|A^c) = (P(JB)+P(JC))/P(B+C) = (1/3)/(2/3) = 1/2
    P(J^c|A^c) = 1/2
(c) P(A|J) = P(J|A)P(A)/P(J) = 1/2 * 1/3 / 1/2 = 1/3
(d) P(A|J) = 1 * 1/3 / 2/3 = 1/2
    (P(J) = P(J|B)P(B) + P(J|B^c)P(B^c)
          = 0 + 1*2/3 = 2/3)

By the way, stackoverflow thread here has the wrong answer:

https://stats.stackexchange.com/questions/17334/the-prisoner-paradox
-}

----------------------------------------------------------------------------
-- 5.27 Monty hall problem
----------------------------------------------------------------------------

{-
(a) W - initial choice is correct. V - final choice is correct.

Scenario A (stick): p(V|W) = 1, P(V|W^c) = 0
p(V) = P(V|W)p(W) + P(V|W^c)p(W^c) = 1 * 1/n + 0 = 1/3

Scenario B (switch): p(V|W) = 0, P(V|W^c) = 1
p(V) = P(V|W)p(W) + P(V|W^c)p(W^c) = 0 + 1 * (n-1)/n

(b) ¯\_(ツ)_/¯

Please don’t say “You are lazy”
だって本当はcrazy
白鳥たちはそう
見えないとこでバタ足するんです

(c)

A - initial choice is correct, B - final choice is correct.
P(B) = P(B|A)P(A) + P(B|A^c)P(A^c)

In both cases P(A) = m/n (guess the car), P(A^c) = 1 - m/n = (n-m)/n (not guess it).

Scenario 1 (stick):
P(B|A) = 1
P(B|A^c) = m/(n-k-1) :
guess m cars from n-k-1 places (still open, that are not the chosen one) left.

Then P(B) = 1*m/n + m/(n-k-1)*(n-m)/n
          = m(2n - m - k - 1)/(n(n-k-1))

Scenario 2 (switch):

We switch with even probability over left places.

P(B|A) = (m-1)/(n-k-1)
there are m-1 cars left there, because we've guessed one
P(B|A^c) = m/(n-k-1)

P(B) = (m-1)/(n-k-1)*m/n + m/(n-k-1)*(n-m)/n
     = m(n - 1) / (n(n-k-1))

The difference (n-k-1 is positive):

δ = P(B)_switch - P(B)_stick =
  (n - 1 - 2n + m + k + 1) = m + k - n

If k = n-2, m = 1, then δ = 1 - n + n - 2 = -1

Which is crap.

-}

----------------------------------------------------------------------------
-- 5.28 Generic Monte Carlo
----------------------------------------------------------------------------

{-
B = {m has some property}
A = {algorithm returned 1}

Let's see what we know:
P(B) = 1-δ, P(B^c) = δ
P(A|B) ≥ p
P(B|A) = 1
P(A|B^c) = 0
P(A^c|B^c) = 1

P((A^c)^N|B) = {calculations} ≤ (1-p)^N

P(B^c|(A^c)^N) = P((A^c)^N|B^c)P(B^c) / (P((A^c)^N|B)P(B) + P((A^c)^N|B^c)P(B^c))
               ≥ 1 * δ / ((1-p)^N * (1-δ) + 1 * δ)
               = δ / [ (1-p)^N * (1 - δ) + δ ]

-}

----------------------------------------------------------------------------
-- 5.29 Monte Carlo calculations
----------------------------------------------------------------------------

monteCarlo :: Double -> Double -> Integer -> Double
monteCarlo d p n = d / ((1-p)^n * (1 - d) + d)

{-
(a)
λ> monteCarlo (9/10) (3/4) 25
0.9999999999999999

(b)
λ> monteCarlo (9/10) (3/4) 100
1.0 -- for sure, man (hehe)

(c)
1 time is enough:
λ> monteCarlo (99/100) (1/2) 1
0.9949748743718593

(d)
7 is the first one:
λ> monteCarlo (99/100) (1/2) 6
0.9998421966230078
λ> monteCarlo (99/100) (1/2) 7
0.9999210920855361
-}

----------------------------------------------------------------------------
-- 5.30
----------------------------------------------------------------------------

{-
B = {m has property "being composite"}
A = {Miller-Rabin test says "Yes" ~ "Found compositeness witness"}

The task is to estimate (B^c|(A^c)^N), which is a direct application of the 5.28.

P ≥ δ / [ (1-p)^N * (1 - δ) + δ ]
  = 1/ln(n) / [ (1/4)^N * (1-1/ln(n)) + 1/ln(N) ]
  = 1/ln(n) / [ (1/4)^N * (ln(n)-1) / ln(n) + 1/ln(N) ]
  = 1 / [ (1/4)^N * (ln(n)-1) + 1 ]
  = 1 / [ (1/4)^N * ln(n) - (1/4)^N + 1 ]
  = 1 / [ (1/4)^N * ln(n) + (1 - 4^(-N)) ]
  = 1 / [ (1/4)^N * ln(n) + (1 - 2^(-(N+1))) ]

-}

----------------------------------------------------------------------------
-- 5.31
----------------------------------------------------------------------------

{-
Correlation ≠ causation.
-}

----------------------------------------------------------------------------
-- 5.32
----------------------------------------------------------------------------

{-
f_X(k) = (n choose k) p^k (1-p)^{n-k}

∑{k=0..n}(f_X(k))
  = ∑ (n choose k) p^k (1-p)^{n-k}
  = (p + (1-p))^n = 1
-}
