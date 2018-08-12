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
-}
