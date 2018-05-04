{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | Weil pairing over fields of prime power order, distortion map and
-- modified pairing.

module Module6v9 (e645) where

import Universum hiding ((<*>))

import qualified Data.HashSet as HS
import qualified Data.List as L
import Data.Numbers.Primes (isPrime, primes)

import Lib.Elliptic
import Lib.Field
import Lib.Misc
import Module6v8 (millerWeil)

----------------------------------------------------------------------------
-- 6.38 Distortion map on the supersingular curve
----------------------------------------------------------------------------

{-

φ: (x,y) ↦ (-x,αy)

x(φ(P)+φ(P)) = ((3(-x)^2 + A)/2(αy))^2 - 2(-x)
             = -((3x^2 + A)/2y)^2 + 2x
             = x(φ(P+P))

y(φ(P)+φ(P)) = ((3(-x)^2 + A)/2(αy))(x + ((3x^2 + A)/2y)^2 - 2x) - αy
             = ((3(x)^2 + A)/2(αy))(x + ((3x^2 + A)/2y)^2 - 2x) - αy
             = α(((3(x)^2 + A)/2((α^2)y))(x + ((3x^2 + A)/2y)^2 - 2x) - y)
             = α(-((3(x)^2 + A)/2y)(x + ((3x^2 + A)/2y)^2 - 2x) - y)
             = α(((3(x)^2 + A)/2y)(x - ((3x^2 + A)/2y)^2) - y)
             = α(y(P+P))
             = y(φ(P+P))

-}

----------------------------------------------------------------------------
-- 6.39 Torsion group with l > sqrt(p)+1
----------------------------------------------------------------------------

{-
Otherwise (E(F_p)[l] ≅ (Z/lZ)^k, k ≥ 1) it'd have more elements then
the original field which is impossible.
-}

----------------------------------------------------------------------------
-- 6.40 Using distortion map to solve EDDHP
----------------------------------------------------------------------------

{-
P, aP, bP, cP ∈ E(F_q)[l].

φ : E[l] → E[l] is a distortion map.

Let e be the modified Weil pairing with respect to φ.

Well, e'(aP,bP) would be equal to e'(P,cP) since

  e'(aP,bP) = e(P,φ(P))^{ab} = e(P,φ(abP)) = e'(P,cP)
-}

----------------------------------------------------------------------------
-- 6.41
----------------------------------------------------------------------------

{-
Trivial:

φ((x,y)) = (-x,αy)
φ((-x,αy)) = (x,-y)
-}

----------------------------------------------------------------------------
-- 6.42
----------------------------------------------------------------------------

{-

l = 3 (mod 4). This gives us a possibilty to compute roots effectively in F_l.
But is it really needed? :think:

Also, p - l = 0 (mod 4)

To prove: e_l(Q,φ(Q))^l = 1 for every Q ∈ E[l].

Let's use proposition 6.50 (b) also. So we're trying to prove that Q
is not multiple of P. Let φ(P) be multiple of P.


TODO: I don't understand why the proof in prop. 6.53 doesn't work

-}

----------------------------------------------------------------------------
-- 6.43 Distortion map for y^2 = x^3 + 1
----------------------------------------------------------------------------

{-
β ∈ K, β ≠ 1, β^3 = 1;
φ(x,y) = (βx,y), φ(0) = 0;

(a) P = (x,y) ∈ E(K), then φ(P) = (βx,y), then (βx)^3 = x^3, and
hence y^2 = (βx)^3 + 1 still holds.

(b) Same as in 6.38

x(φ(P1)+φ(P2)) = [ (y2-y1)/(βx2-βx1) ]^2 - βx1 - βx2
               = (y2-y1)^2/β^2(x2-x1)^2 - β(x1 - x2)
               = β [ (y2-y1)^2/(x2-x1)^2 - x1 - x2 ]
               = x(φ(P1+P2))

y(φ(P1)+φ(P2)) = [3β^2x1^2 + A / 2y1](βx1 - βx3) - y1
               = [3β^3x1^2 + A / 2y1](x1 - x3) - y1
               = [3x1^2 + A / 2y1](x1 - x3) - y1
               = y(φ(P1+P2))

Should also work for P1 = P2.

-}

----------------------------------------------------------------------------
-- 6.44 ... more
----------------------------------------------------------------------------


primesINeed = filter (\x -> x `mod` 3 == 2) primes

{-
(a) p ≥ 3 is a prime with p ≡ 2 (mod 3).

1. F_p doesn't contain β (β^3 = 1). In group with p ≡ 2 (mod 3) every number
   has a cubic residue (wiki, ex 3.41 also): x = (x^{2n+1})^3 for p = 3n+2.
   But the only cubic residue for 1 is 1 itself, which contradicts the
   requirement for β not to be equal to 1.

2. F_{p^2}.

   I was trying to find an irreducible polynomial for any p with degree 2, but
   in order to do this, ax^2 + bx + c shouldn't have roots, but it seems
   that there exists no F_p element that would not have a root for all
   the values of p. So different p means different irreducible polynomials.
   EDIT: in fact, 3n-1 never has a square root if p ≡ 2 (mod 3), then we
   should represent 3n-1 as b^2-4ac -- we can take b = 1, a = 1, c = -(3n-2)/4,
   so Δ = 1 + (3n-2) = 3n-1. But it only makes things complicated, I think.


   It appears that p = 3n+2 splits into two classes with modulo 4: 1 and 3.

   If p ≡ 3 (mod 4) (additionally to p ≡ 2 (mod 3)), then we're in complex plane
   and (a + ib)^3 = (a^3 - 3ab^2) + (3a^2b - b^3)i.

   We want real part to be 1 and imaginary to be 0, so we solve
     a = (-1/8)^{1/3}
     b = sqrt(3) * (-1/8)^{1/3}

   We can always take inverse, we can negate and we can take cubic roots.
   Quadratic reciprocity for 3: (3,p) ⇒ -(p,3) = -(2,3) = -(-1) = 1, so
   we can always take a sqrt(3).

   If p ≡ 1 (mod 4), apply similar considerations.


(b) Same as in proposition 6.53: φ(P) = (βx,y), φ(P) has the same order as P
    since lφ(P) = φ(lP) = 0.

    Let's assume Q = φ(P) is a multiple of P. F_p doesn't contain β,
    so x = 0, but then P = (0,y) can be only (0,1) and (0,-1), but these
    points have order 3, so Q is not multiple of P and we're done.

    P = (0,-1) has order 3 since 2P = (0,1) [easy to verify: λ = 0,
    so x3 = 0 and y3 = 0 - y1 = 1] and 3P = (0,1) + (0,-1) = 0.

    P = (0,1) is similar: 2P = (0,-1), 3P = 0.
-}

-- Checks that (0,1) and (0,-1) have order 3 for some primes to give an intuition.
e644Check :: IO ()
e644Check = do
    let solve :: forall n. (PrimeNat n) => IO ()
        solve = withECParams (ECParams (toZ 0) (toZ 1) :: ECParams (Z n)) $ do
            let threePowers (p :: ECZ n) = do
                    print $ p
                    print $ 2 `times` p
                    print $ 3 `times` p
                    putText "---"
            threePowers (EC 0 1)
            threePowers (EC 0 (-1))
    solve @17
    solve @19
    solve @23
    solve @631
    solve @883
    solve @1009

----------------------------------------------------------------------------
-- 6.45 Computing modified Weil pairing
----------------------------------------------------------------------------

-- finpoly ex645
type FP645 = FinPoly 477482 (Z 691)
--type FP645 = FinPoly 362 (Z 19)

-- The faster version of findGeneratorH usnig hashsets.
findGeneratorH ::
       forall p a. (Eq a, Ord a, Hashable a, AGroup a)
    => (FinPoly p a -> FinPoly p a -> FinPoly p a)
    -> [FinPoly p a]
    -> IO (FinPoly p a)
findGeneratorH op elems0 = do
    --elems <- shuffleM elems0
    let weightsSum (FinPoly (Poly l)) = foldl (<+>) f0 l
    let elems = sortBy (comparing weightsSum) elems0
    pure $ fromMaybe (error $ "findGenerator: didn't manage to") $
      find (\g -> let s = genOrderSet HS.empty g g in HS.size s == n) elems
  where
    n = length elems0
    genOrderSet acc g0 g | g `HS.member` acc = acc
                         | otherwise = genOrderSet (HS.insert g acc) g0 (g `op` g0)

-- We're actually working in complex plane, 691 = 3 (mod 4), so
e645 :: IO ()
e645 = do
    let fpl :: [Integer] -> FP645
        fpl = mkFinPoly . Poly . map toZ
    withECParams (ECParams (fpl [0,1]) (fpl [0]) :: ECParams FP645) $ do
        --g <- findGeneratorH (<*>) (L.delete f0 (allElems @FP645))
        let g = fpl [3,689] -- pre-computed generator
        let α = g <^> ((691^2-1) `div` 4)
        let phi (EC x y) = EC (fneg x) (α <*> y)
            phi EC0      = EC0
        let e m p (phi -> q) =
                let s = 6 `times` p <+> 3 `times` q
                in millerWeil m p q s
        let p = EC (fpl [301]) (fpl [14])

        let e' = e 173 p p
        print $ e'
        print $ e' <^> 173
