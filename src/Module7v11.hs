{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | NTRU as lattice cryptosystem.

module Module7v11 () where

import Universum hiding ((<*>))

import Lib.Field
import Lib.Vector

----------------------------------------------------------------------------
-- 7.36 NTRU as CVP
----------------------------------------------------------------------------

{-

(a) To prove: (pr, e-m) is in NTRU lattice.
Let's use proposition 7.59: pr⋆h = e-m, so pr⋆h = e-m + qu for some u,
and thus (pr, e-m) ∈ L.

(b) let w = (pr, e-m) - (0,e) = (pr, -m)
    ∥w∥ = {at most} = sqrt(2d * p^2 + p^2/2 * N) = 1.08p*sqrt(N)
    We want ∥w∥/σ(L) = 1.288p/sqrt(N) < 1, which implies
    1.66p^2 < N

    But q > (6d+1)p, which leads to p < N/(N+0.5) = S

    Let's substitute S in the previous inequation as maximum to show
    that even in this case the inequation holds.

    1.66S^2 < N
    1.66N^2/(N + 0.5)^2 < N

    this holds for every N > 0, so yes indeed ∥w∥ < σ(L)

(c) It is easy to show that the proposition similar to 7.59 holds in this
modified lattice: if f(x)⋆(ph(x)) = g(x)  (mod q), then (f,g) ∈ L. Thus,
when e = pr*h + m, v' = (r, e-m) ∈ L. Since the length of v'-(0,e) is obviously
less than length of (pr,-m), security of the scheme decreases, as it was
previously said that the smaller the smallest vector with respect to the
gaussian heuristics, the easier it is to be found (remark 7.62).

-}

----------------------------------------------------------------------------
-- 7.37
----------------------------------------------------------------------------

{-

R = Z[x]/(x^N-1)
R2 = Z[x]/(x^{N/2}-1)

For a we'll use the notation a0 to say a (in R2), and x1 to mean x^{N/2} * a (in R2),
so N/2 lower and higher coefficients.

Next, as it's already noticed, if (f,g) is a solution to a key recovery problem
(fh = g), then (x^i*f, x^i*g) is too.

So let's suppose that fh = g, and we're looking for (f,g).

First, expanding fh = g:
f0h0 = g0 + x^{N/2} * (f1h0 + f0h1 + g1)

Let's expand x^Nfh = x^Ng:
f1h1 = g1 + x^{N/2} * (f0h1 + f1h0 + g0)

We then reduce modulo x^{N/2} - 1 to get these two equations:

(f0+f1)h0 = g0+g1+f0h1
(f0+f1)h1 = g0+g1+f1h0

Then we solve for ah0 = b and ch0 = d. If a /= c, we multiply one
of these (either a or c) by powers of x to get a = c. Let's suppose that
a = c, so now we have these three variables: a, b, d.

f0+f1 = a
g0+g1+f0h1 = b
g0+g1+f1h0 = d

It's easy to see that when we subtract (2) from (3) (or vice versa), we get
system with two equations that allows us to restore f:

f0 + f1 = a
f0h1 - f1h0 = b - d

After we find f, g = f*h.

-}

----------------------------------------------------------------------------
-- 7.38
----------------------------------------------------------------------------

{-

If we know t coefficients of m, we should first rearrange the basis so that
the known t coefficients are the last one. It is surely possible to do so,
it's easy to construct the basis transformation matrix (it only has a single
one in each row or column).

Then we act like in 7.37:

let M = N - t
for any a let a  = a0 + x^M*a1 (the real a divided into two parts),
              a' = a0 + a1     (first part of a plus second part modulo x^M-1)

e = prh + m
e0+x^Me1 = p(r0+x^Mr1)(h0+x^Mh1) + m0 + x^Mm1         (notice: we don't know r0,r1,m0)
e0+e1 = p(r0+r1)(h0+h1) + m0 + m1                     (reduced modulo x^M-1)
e' = p*r'*h' + m0 + m1                                (apply notation)
e'-m1 = p*r'*h' + m0                                  (rearrange)

This last equation looks exactly like CVP we can solve in lattice of size 2*M.
Finding closest vector to (0,e'-m1) will yield some (a,b), where

a = pr'
b = e'-m1-m0

So we've recovered m0 = b - e' + m1

-}
