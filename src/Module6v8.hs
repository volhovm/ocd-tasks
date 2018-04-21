-- | Bilinear pairings on EC.

module Module6v8 () where

import Universum hiding ((<*>))

import qualified Data.List as L

import Lib.Elliptic
import Lib.Field
import Module6v3 (binExpand)

----------------------------------------------------------------------------
-- 6.29
----------------------------------------------------------------------------

{-
div(f(x)) is sum of all zeroes minus sum of all poles. Naturally,
R(x)S(x) has all zeroes of R(x) and all zeroes of S(x), same applies
to the poles.
-}

----------------------------------------------------------------------------
-- 6.30
----------------------------------------------------------------------------

{-

(a) Y^2 = (X-α1)(X-α2)(X-α3), so if P = (α,0) ∈ E, then α should be equal to
one of these three -- there are no other points with Y = 0. On the other hand,
we do not know how many times X = α intersects EC, but it does it at least once.

(b) The EC hypothesis is that it has three distinct roots with Y = 0, so
multiply (X-αi) and (X-αj) to get (X^2 + aX + b).

(c) Divisor of y^2 is div(∑(X-α_i)) = 2[P1] + 2[P2] + 2[P3] - 6[0]
    Divisor of (X^2 + aX + b) is 2[P2] + 2[P3] - 4[0] (indices 2 and 3 are
    choosen randomly, could be any other pair).

    So then div(X-α) = 2[P1] - 2[0]

(d) ???

The exercise is pretty straightforward, I have no idea why this particular
proof method is chosen in the exercise. X - α is a vertical line, it either
has two intersections or one. Since P = (α,0) ∈ E, this root has multiplicity 2.

More details (the description in the book is poor imho):
https://crypto.stanford.edu/pbc/notes/elliptic/divisor.html
https://math.stackexchange.com/questions/511950/calculating-the-divisors-of-the-coordinate-functions-on-an-elliptic-curve?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
-}

----------------------------------------------------------------------------
-- 6.31
----------------------------------------------------------------------------

{-

Easy (given that we have proved alternating and bilinear properties):

  1 = e(P+Q,P+Q) = {bilinearity} = e(P,Q)e(Q,P)

A good article on Weil pairing properties:
http://www.mathematik.uni-regensburg.de/kerz/ss16/ausarb/sotakova.pdf

-}

----------------------------------------------------------------------------
-- 6.32
----------------------------------------------------------------------------

{-
(a) Functions with the same divisor only differ by a constant (theorem 6.36),
so choosing any other f_p or f_q will not make any effect, as this constant
will cancel itself.

(b) Let's calculate divisor of F(S)

    div(f_P(Q+S)) = m[-Q+P] - m[-Q]
    div(f_P(S))   = m[P] - m[0]
    div(f_Q(P-S)) = m[P-Q]  - m[P]
    div(f_Q(-S))  = m[-Q] - m[0]

    div(F(S)) = (sum these all up) = 0, thus (according to theorem 6.36),
    F(S) is constant.
-}

----------------------------------------------------------------------------
-- 6.33
----------------------------------------------------------------------------

{-
em(P,Q) = em(apP1 + bpP2, aqP1 + bqP2)
        = em(P1,P2)^{aP*bQ} * em(P2,P1)^{bP*aQ}
        = em(P1,P2)^{aP*bQ - bP*aQ}
-}

----------------------------------------------------------------------------
-- 6.34 (this is an excercise for section 6.9, bug)
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- 6.35 Miller's algorithm
----------------------------------------------------------------------------

-- Computes Weil pairing of power m for P and Q.
millerWeil :: forall f. (FField f, HasECParams f, Show f) => Integer -> EC f -> EC f -> EC f -> f
millerWeil m p q s = do
    let calc b arg = loop (drop 1 $ reverse $ binExpand m) arg b f1
    let e1 = calc p (q <+> s)
    let e2 = calc p s
    let e3 = calc q (p <-> s)
    let e4 = calc q (fneg s)
    traceShow e1 $
     traceShow e2 $
     traceShow e3 $
     traceShow e4 $
     e1 <*> e4 <*> finv (e2 <*> e3)
  where

    loop :: [Integer] -> EC f -> EC f -> f -> f
    loop [] _ _ f = f
    loop mi x t f = do
        let t' = 2 `times` t
        let f' = (f <^> 2) <*> g t t x
        uncurry (loop (L.tail mi) x) $ case L.head mi of
          1 -> (t' <+> p, f' <*> g t' p x)
          _ -> (t', f')

    g :: EC f -> EC f -> EC f -> f
    g p'@(EC xp yp) q'@(EC xq yq) (EC x y) =
        let ECParams{..} = ecParams
            λ = if xp == xq
                then (3 `times` (xp <^> 2) <+> ecA) <*> finv (2 `times` yp)
                else (yq <-> yp) <*> finv (xq <-> xp)
        in if p' == q'
           then x <-> xp
           else (y <-> yp <-> λ <*> (x <-> xp)) <*>
                finv (x <+> xp <+> xq <-> λ <^> 2)
    g _ _ _ = error "millerWeil: g called with zero point"

e635 :: IO ()
e635 = do
    let solve :: forall n. (PrimeNat n) =>
                 Integer -> Integer -> ECZ n -> ECZ n -> ECZ n -> Integer -> IO ()
        solve a b p q s m =
            withECParams (ECParams (toZ a) (toZ b) :: ECParams (Z n)) $ do
                let w = millerWeil m p q s
                if w <^> m == 1 then print w else error "e635 whoops"

    solve @631 30 34 (EC 36 60) (EC 121 387) (EC 0 36) 5

--    solve @1051 0 23 (EC 109 203) (EC 240 203) (EC 1 554) 5
--    solve @883 (-35) (-9) (EC 5 66) (EC 103 602) (EC 1 197) 7
--    solve @1009 0 37 (EC 8 703) (EC 49 20) (EC 0 0) 7
--    solve @1009 0 37 (EC 417 952) (EC 561 153) (EC 0 0) 7

{-
λ> e635
498
522
129
202
-}
