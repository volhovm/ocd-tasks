-- | Information theory.

module Module5v6 () where

----------------------------------------------------------------------------
-- 5.45
----------------------------------------------------------------------------

{-
https://cdn-images-1.medium.com/max/455/1*snTXFElFuQLSFDnvZKJ6IA.png

(a) f(c1) = 1/3 (2/5 + 2/5 + 1/5) = 1/3
    f(c2) = 1/3 (2/5 + 1/5 + 1/5) = 4/15
    f(c3) = 1/3 (2/5 + 2/5) = 4/15
    f(c4) = 1/3 2/5 = 2/15

(b) f(c1|m1) = 1/3, equal to f(c1)
    f(c1|m2) = 1/3, not equal to f(c2)
    f(c1|m3) = 1/3, not equal to f(c3)
    No perfect secrecy ofc.

(c) f(c2|m1) = 1/3, f(c3|m1) = 1/3

(d) f(k1|c3) = f(c3|k1)f(k1)/f(c3) = 0.
    f(k2|c3) = 2/5 * 1/3 / 4/15 = 2/15 / 4/15 = 1/2
-}

----------------------------------------------------------------------------
-- 5.46
----------------------------------------------------------------------------

{-
I will assume that the described system (encrypting strings) is
isomorphic to the system, encrypting single letters. Then:

(a) f_M(d_k(c)) = 1/26, so the sum is 1. Every k provides a bijection
between C and M, so we're good here.

(b) f(c) = 1/26 * sum({a}) = 1/26 * 1 = 1/26

(c) f(c|m) = f(c,m)/f(m) = sum(f(c,m|k)f(k)) / f(m)
           = {there's only 1 k encrypting m into c}
           = f(c,m|k_0)f(k_0) / f(m) = (1/26)(1/26)/(1/26) = 1/26

-}

----------------------------------------------------------------------------
-- 5.47
----------------------------------------------------------------------------

{-
f(c) = sum{k}(f(c|k)f(k))
     = sum{k | c ∈ e_k(M)}(f(c|k)f(k))            for other ks f(c|k) = 0
     = sum{..}(f(d_k(c)|k)f(k))                   C is determined by M
     = sum{..}(f(d_k(c))f(k))                     M and K are independent (?)
-}

----------------------------------------------------------------------------
-- 5.48
----------------------------------------------------------------------------

{-
Otherwise, d_k(c) is a two-value function, which (i guess) leads to
contradiction. #M = #C is given because if #C < #M, we can't make a
bijection. Similar to Claim 1 in the theorem 5.56.
-}

----------------------------------------------------------------------------
-- 5.49
----------------------------------------------------------------------------

{-
k ∈ S_{m,c} ∩ S_{m,c'}, then e_k(m) = c = c', so we've managed to come
with two ciphertexts for a single message. That's weird, what kind of
function e_k(m) is then?
-}

----------------------------------------------------------------------------
-- 5.50
----------------------------------------------------------------------------

{-
In the end of theorem 5.56 we prove that f(c) is constant. We'll use
the (b) property of the theorem to deduce f(m) is constant, since f(k)
and f(c) are constant and (b) says that ∀m,c∃!k. Also 5.48 says that m
is uniquely determined by (k,c).
-}

----------------------------------------------------------------------------
-- 5.51
----------------------------------------------------------------------------

{-
Let #K = #M = #C = N.

1. Since (c,k) ↦ m, sum{k ∈ K}(f(d_k(c))) = sum{m ∈ M}f(m) = 1.
2. Directly apply 5.46(b): since all keys are used with the same probability,,
   f_C(c) = ∑(f(k)f(d_k(c))) = f(k_0)∑{..} = f(k_0) = 1/N.
3. f(c|m) = f(c,m)/f(m) = ∑f(c,m|k)f(k) / f(m)
   Recall, that f(c,m|k) = f(m|k), because c is defined by (m,k); then:

   f(c|m) = ... = f(k)∑f(m|k) / f(m)

   but ∑f(m|k) = f(m), since there's only one k that encrypts m into c.

   so f(c|m) = f(k) = 1/N, which equals to f(c).
-}

----------------------------------------------------------------------------
-- 5.52
----------------------------------------------------------------------------

{-
Just apply induction. We'll choose Y as X_{n^(r-1)}, and when we
choose one of n^(r-1) functions, each chooses n.

H(X_{n^r}) = H(X_{n^(r-1)}) + ∑1/n H(X_n)
           = (r-1)H(X_n) + H(X_n)
           = rH(X_n)
-}

----------------------------------------------------------------------------
-- 5.53
----------------------------------------------------------------------------

{-
Trivial, done in 3 transformations in RHS: unite under one sum, add
∑qij, unite again and sum logs.
-}

----------------------------------------------------------------------------
-- 5.54
----------------------------------------------------------------------------

{-
Let b = a + ε, then we achieve local concavity at every point. This is
easy to see, since f''(x) < 0 means that f'(x) decreases, so f'(b) <
f'(a).

More generally, let b = a + δ, let's se what f'(a) - f'(b) = s > 0 means:

(f(a+ε) - f(a) - f(a+δ+ε) + f(a+δ)) / ε → s

Let's take δ = ε.

(2f(a+ε) - f(a) - f(a+2ε)) / ε → s

If you draw a plot (I wanted to do some ASCII art, excuse me):

   ^            o
   |     o
   |
   |
   | o
   |
   \_________________>
     a   a+ε   a+2ε

This means that the middle is higher than arithemtic average of a and
a+2ε, which can be easily translated into α-based definition.

So now we have this local concavity, how do we prove it for b = a + δ,
where δ is something non-ε? f(x) continuity helps: just partition your
δ into εs, such that every small interval is positive, and then
combine.


,(   ,(   ,(   ,(   ,(   ,
  `-'  `-'  `-'  `-'  `-'
     ////
    !!!!  _      \\\\
    !   -'/   _  ||||
     \   /    \`-'''|
      \  |     \   /
      )  |      \  \
      /  |       \  \
     /   |        \  \
,(   ,(   ,(   ,(   ,(   ,
  `-'  `-'  `-'  `-'  `-'


Log part is done easily then: (log_b(x))'' = -1/(x^2 * lnb), which is
< 0 for every x.
-}

----------------------------------------------------------------------------
-- 5.55
----------------------------------------------------------------------------

{-
F(Σ(ai*ti)) = F(an*tn - (1-an)∑{i=1..n-1}(a_i*ti/(1-an)))
            ≥ ...
            = an*F(tn) + (1-an)[∑{n-1}ai/(1-an)*F(tn)]
            = ∑{n}aiF(ti)
-}

----------------------------------------------------------------------------
-- 5.56
----------------------------------------------------------------------------

{-
(a) H(X|Y) = ∑p(y)∑(p(x|y)log(p(x|y))) = ∑p(y)H(X) = H(X) * ∑p(y) = H(X)
(b) p(x)log(p(x)) = p(x|y)log(p(x|y)) for any y.
-}

----------------------------------------------------------------------------
-- 5.57
----------------------------------------------------------------------------

{-
H(X|Y) = H(XY) - H(Y), but H(XY) = H(Y|X) + H(X), so
H(K|C) = H(C|K) + H(K) - H(C)

So H(C|K) must be H(M).

H(C|K) = ∑{k∈K} p(k) * ∑{c∈C} p(c|k)log(p(c|k))
       = ∑{k∈K} p(k) * ∑{c∈(reachable part of C)} p(c|k)log(p(enc|k))   because maybe some c can't be produced by e_k(m)
       = ∑{k∈K} p(k) * ∑{m∈M} p(e_k(m)|k)log(p(e_k(m)|k))               same
       = ∑{k∈K} p(k) * ∑{m∈M} p(m|k)log(p(m|k))                         since m ↦ c
       = ∑{k∈K} p(k) * ∑{m∈M} p(m)log(m)                                since we choose m and k independently
       = H(M)
-}

----------------------------------------------------------------------------
-- 5.58
----------------------------------------------------------------------------

{-
(a) H(K) = log(1/3) = 1.585
    H(M) = 1.522
    H(C) = 1.933
(b) H(K|C) = 1.174
-}

----------------------------------------------------------------------------
-- 5.59
----------------------------------------------------------------------------

{-
If there's at least one ciphertext and H(K|C) = 0, then it must be
that log(p(k|c)) = 0, then p(k|c) = 1.
-}
