{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | EC over p^k

module Module6v7 () where

import Universum

import Control.Lens (at, (.=))
import qualified Data.Map as M

import Lib.EllipticFull
import Lib.Field

----------------------------------------------------------------------------
-- 6.22 EC Generalized Weierstrass addition
----------------------------------------------------------------------------

{-
We will proceed as in theorem 6.6. Let's suppose that addition on general
weierstrass curve works the same (take intersection of a line with a curve)
as on simplified curve.

P1 = (x1,y1), P2 = (x2,y2)

1. Points are different.

λ = y2-y1 / x2-x1 is a slope of a tangent line, y = λx + φ is its equation.
Let's substitute this y into weierstrass equation.

x^3 + (a2 - a1λ - λ^2)x^2 + (...)x + (...) = 0

We know that this cubic has three roots, also two of them are already known
(x1, x2). Let's compare coefficients of x^2:

a2 - a1λ - λ^2 = - x1 - x2 - x3
x3 = a1λ + λ^2 - a2 - x1 - x2

This looks exactly as expected. Then,

y3' = λ*x3 + φ, where

φ = y1 - λx1
  = y1 - x1(y2-y1)/(x2-x1)
  = [y1(x2-x1) - x1(y2-y1)]/(x2-x1)
  = (y1x2 - x1y2)/(x2-x1)
  = ν

Since the inversion of y is y ↦ -y - a1x - a3, we get inverse of y3':

y3 = - λx3 - ν - a1x - a3

which is same as expected.

2. Points are the same.

First, we find a slope of the tangent line.

y^2 + a1xy + a3y = x^3 + a2x^2 + a4x + a6
(y^2 + a1xy + a3y)' = (x^3 + a2x^2 + a4x + a6)'
2y*y' + (a1x*y' + a1*y) + a3*y' = 3x^2 + a2x + a4
y'(2y + a1x + a3) = 3x^2 + a2x + a4 - a1y
y' = (3x^2 + a2x + a4 - a1y) / (2y + a1x + a3)

λ = (3x1^2 + a2x1 + a4 - a1y1) / (2y1 + a1x1 + a3)

Similarly,

y = λx3 + φ, where φ = y1 - λx1 = ν (as defined)

-}

----------------------------------------------------------------------------
-- 6.23
----------------------------------------------------------------------------

type E623F = FinPoly 11 (Z 2)

e623 :: IO ()
e623 = do
    let mkp = mkFinPoly . Poly
    let params :: ECParams E623F
        params = ECParams f1 f1 f1 (mkp [1,0]) (mkp [1,1])
    withECParams params $ do
        print $ discriminant params
        let (p :: EC E623F) = EC (mkp [1,1,1]) (mkp [1,1])
        let q = EC (mkp [1,0,0]) (mkp [1,0])
        let (r :: EC E623F) = EC (mkp [1,1,1]) (mkp [1,0,1])
        print $ p <+> q
        print $ 2 `times` r
        print $ listAllPointsSlow @E623F
        print $ getGen @E623F


{-
FinPoly [1,0,0]
EC FinPoly [1,1,0] FinPoly [1,0]
EC FinPoly [1,0,1] FinPoly [1,0,0]
[EC0,EC FinPoly [1] FinPoly [1],EC FinPoly [1,0] FinPoly [0],EC FinPoly [1,0] FinPoly [1,1],EC FinPoly [1,1,0] FinPoly [1,0],EC FinPoly [1,1,0] FinPoly [1,0,1],EC FinPoly [1,1,1] FinPoly [1],EC FinPoly [1,1,1] FinPoly [1,1,1]]
FinPoly [1,0]
-}

----------------------------------------------------------------------------
-- 6.24
----------------------------------------------------------------------------

{-
(a) As in 5.8, we use the fact that (p choose i) divides p, and so, application
of binomial theorem only leaves a^p + b^p and all other elements are gone, since
they are kp * s = 0 for some polynomial s.

Multiplication follows trivially from the field commutativity.

(b) Fermat's little theorem.

(c) τ is a F_(p^k) homomorphism, so applying it to any operation that is defined
in F_(p^k) will preserve the function:

τ(f(P,Q)) = f(τ(P), τ(Q))
-}

----------------------------------------------------------------------------
-- 6.25 Trace of frobenius for Kobeltz curve
----------------------------------------------------------------------------

{-

Let's call this curve E0 as in definition of Koblitz curve in the textbook.
We already know that #E0(F2) = 4, so by the theorem 6.29
 #E0(F_(p^k)) = 2^k + 1 - α^k - β^k


ti = 2^k + 1 - #E0(F2^k)
   = 2^k + 1 - 2^k - 1 + α^k + β^k
   = α^k + β^k
   = [(-1 + sqrt(-7)) / 2] ^ k +
     [(-1 - sqrt(-7)) / 2] ^ k

(a)

It's easy to show that

t1 = -1/2 -1/2 = -1

t2 = (1 + 2sqrt(-7) - 7) / 4 + (1 + 2sqrt(-7) - 7)
   = -12 / 4
   = -3

(b)

Let s = (-1 + sqrt(-7)) / 2, p = { same but with - }

Then formula is:

(s + p)(s^(k-1) + p^(k-1)) - 2(s^(k-2) + p^(k-2))
  = s^k + p^k + sp^(k-1) + ps^(k-1) - 2(s^(k-2) + p^(k-2))
  = s^k + p^k + spp^(k-2) + sps^(k-2) - 2(s^(k-2) + p^(k-2))
  = s^k + p^k + sp(p^(k-2) + s^(k-2)) - 2(s^(k-2) + p^(k-2))
  = s^k + p^k + (sp - 2)(s^(k-2) + p^(k-2))                     # sp = 2
  = s^k + p^k
  = t_k

-}

-- worked terribly slow without memoization
e625 :: Integer -> Integer
e625 k0 = fst $ runState (go k0) (M.empty :: Map Integer Integer)
  where
    retrieve :: Integer -> State (Map Integer Integer) Integer
    retrieve x = use (at x) >>= \case
        Nothing -> do y <- go x
                      at x .= Just y
                      pure y
        Just y -> pure y
    go 1  = pure $ -1
    go 2  = pure $ -3
    go !k = do
        a <- retrieve (k-1)
        b <- retrieve (k-2)
        pure $ - a - 2 * b

{-
(c)(d)
λ> map e625 [4,11,31,101]
[1,-67,-90707,2969292210605269]

And indeed, as in example after formula (6.12)

λ> 2^97 + 1 - (e625 97)
158456325028528296935114828764
-}

----------------------------------------------------------------------------
-- 6.26 Trace of frobenius for a generic E(F_(p^k))
----------------------------------------------------------------------------

{-

(a)

It's all the same.
Since α,β are solutions to z^2 - tz + p,
α,β = (t +- sqrt(t^2 - 4p)) / 2
αβ = 1/4 ( t^2 - t^2 + 4p ) = p

So as in ex 6.25,

(α + β)(α^(k-1) + β^(k-1)) - p(α^(k-2) + β^(k-2))
  = α^k + β^k + (αβ - p)(α^(k-2) + β^(k-2))            # αb - p = 0
  = α^k + β^k

(b)

t2 = t1^2 - 2p
t3 = t1t2 - pt1 = t1(t1^2 - 2p) - pt1 = t1^3 - 3pt1
t4 = t1t3 - pt2 = t1(t1^3 - 3pt1) - p(t1^2 - 2p)
                = t1^4 - 3p(t1)^2 - pt1^2 - 2p^2
                = t1^4 - 4p(t1)^2 - 2p^2

-}

----------------------------------------------------------------------------
-- 6.27
----------------------------------------------------------------------------

{-
TODO
-}

----------------------------------------------------------------------------
-- 6.28 τ expansion
----------------------------------------------------------------------------

tauExp :: Integer -> [Integer]
tauExp n = go n 0
  where
    go n0 n1
        | n0 == 0 && n1 == 0 = []
        | otherwise =
          let vi = if odd n0
                   then 2 - ((n0 - 2 * n1) `mod` 4)
                   else 0
              n0' = n0 - vi
          in vi : go (n1 - n0' `div` 2) (- n0' `div` 2)

{-

λ> map tauExp [931, 32755, 82793729188]
[[-1,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,0,-1,0,-1,0,-1],
 [-1,0,1,0,1,0,1,0,1,0,0,0,0,0,0,1,0,-1,0,1,0,0,-1,0,0,0,0,0,1,0,0,-1],
 [0,0,1,0,0,0,0,0,1,0,-1,0,-1,0,0,1,0,0,1,0,1,0,0,0,-1,0,0,-1,0,0,1,0,0,0,-1,0,1,0,0,0,-1,0,0,0,1,0,1,0,-1,0,1,0,-1,0,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,-1,0,-1]]

λ> map (length . tauExp) [931, 32755, 82793729188]
[22,32,74]

-}
