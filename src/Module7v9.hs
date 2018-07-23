{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Convolution polynomial rings.

module Module7v9 () where

import Universum hiding ((<*>))

import Data.List ((!!))
import Data.Reflection (reifyNat)

import Lib.Field
import Lib.Vector

-- Convolution product
cProd :: Ring f => Vect f -> Vect f -> Vect f
cProd (Vect a) (Vect b)
    | length a /= length b = error "cProd sizes"
    | otherwise = Vect $ map ck l
  where
    n = length a
    l = [0..n-1]
    inRange x = x >= 0 && x < n
    ck k =
        foldr1 (<+>) $
        map (\(i,j) -> a !! i <*> b !! j)
            [(i,j) | i <- l, j <- l, inRange i, inRange j, (i + j) `mod` n == k]

----------------------------------------------------------------------------
-- 7.23
----------------------------------------------------------------------------

e723 :: IO ()
e723 = do
    let test :: forall p q. (PolyDivisor p q) => Vect (Z q) -> Vect (Z q) -> IO ()
        test a b = do
            let r1 = a `cProd` b
            print r1
            let repack :: Vect (Z q) -> FinPoly p (Z q)
                repack (Vect x) = mkFinPoly (Poly $ reverse x)
            let r2 = repack a <*> repack b
            print r2
            unless (mkFinPoly (Poly $ reverse $ unVect r1) == r2) $ error "e723 error"

    test @349 @7 (Vect [1,1,0]) (Vect [-5,4,2] :: Vect (Z 7))
    test @1027 @4 (Vect [2,2,2,1,-2]) (Vect [-1,3,-3,-3,-3] :: Vect (Z 4))
    test @2189 @3 (Vect [0,1,0,1,0,0,0]) (Vect [0,1,1,0,1,0,1] :: Vect (Z 3))
    test @1025 @2 (Vect [0,0,1,0,0,1,0,1,1,1]) (Vect [1,1,0,1,1,1,0,1,1,1] :: Vect (Z 2))

----------------------------------------------------------------------------
-- 7.24
----------------------------------------------------------------------------

{-
(a) a(1) = 0, then (x-1) | a(x)
Since (Z/qZ) is UFD, so is (Z/qZ)[x], which means there is
factor ai^q, such that ai(1) = 0 and ai(x) is simple.
If degree of ai(x) is 1, then it's x-1, leading to the contradiction.
If degree of ai(x) > 1, then it's either not prime, or it doesn't have
roots from (Z/qZ) (but from the C).

If (x-1) | a(x), then a(x) = (x-1)b(x), hence a(1) = 0.

(b) (x-1) | a(x), and also (x^N - 1)(1) = 0, so (x-1) | (x^N - 1),
thus their gcd is not 1 => a(x) is not invertible.
-}

----------------------------------------------------------------------------
-- 7.25
----------------------------------------------------------------------------

-- stupid implementation b/c fermat algo doesn't work
findInvNaive ::
       forall f p. (Finite (FinPoly p f), Ring (FinPoly p f))
    => FinPoly p f
    -> Maybe (FinPoly p f)
findInvNaive x = find (\y -> y <*> x == f1) (allElems @(FinPoly p f))

e725 :: IO ()
e725 = do
    let a = mkFinPoly (Poly [1,1,0,1]) :: FinPolyZ 245 3
    let b = mkFinPoly (Poly [-1,1,0,1]) :: FinPolyZ 245 3
    print $ findInvNaive a
    print $ findRoots (unFinPoly a)
    print $ findInvNaive b
    print $ findRoots (unFinPoly a)

{-

λ> e725
Nothing
[1]
Just FinPoly [2,2,2,1]

We get nothing in the first poly because its only root is 1, hence by theorem
7.24(b) it's not invertible.
-}

----------------------------------------------------------------------------
-- 7.26
----------------------------------------------------------------------------

e726 :: IO ()
e726 = do
    let solve :: forall p f. (PolyDivisor p f, PrimeNat f) => FinPolyZ p f -> IO ()
        solve x = do
            let roots = findRoots (unFinPoly x)
            print roots
            putText $ bool "1 in root list, can't take inv"
                           (show (invFinPolyFermat x))
                           (f1 `notElem` roots)
    solve (mkFinPoly (Poly [1,0,0,8,3]) :: FinPolyZ 161061 11)
    solve (mkFinPoly (Poly [1,0,2,-3]) :: FinPolyZ 371305 13)
    solve (mkFinPoly (Poly [1,0,2,-3]) :: FinPolyZ 3404825469 23)

{-


-}
