{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Convolution polynomial rings.

module Module7v9 () where

import Universum hiding ((<*>))

import Data.Coerce (coerce)
import Data.List ((!!))

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

{-
λ> e723
Vect {unVect = [4,6,6]}
FinPoly [6,6,4]
Vect {unVect = [1,1,1,3,3]}
FinPoly [3,3,1,1,1]
Vect {unVect = [2,0,2,1,1,2,0]}
FinPoly [2,1,1,2,0,2]
Vect {unVect = [0,1,1,0,1,0,1,0,0,0]}
FinPoly [1,0,1,0,1,1,0]
-}

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
λ> e726
[]
FinPoly [7,8,3,2,3]
[1]
1 in root list, can't take inv
[1,4,18]
1 in root list, can't take inv
-}

----------------------------------------------------------------------------
-- 7.27
----------------------------------------------------------------------------

compInv :: Vect Integer -> Vect Integer -> Integer -> Vect Integer
compInv x xinv k
    | k == 1 = xinv
    | even k = v
    | otherwise = xinv `vplus` v `vminus` (x `cProd` xinv `cProd` v)
  where
    n = length x

    -- Inverse of x modulo (k / 2) * 2
    v :: Vect Integer
    v = half `cProd` (two `vminus` (coerce x `cProd` half))
      where
        two = Vect $ 2 : replicate (n-1) 0
        half :: Vect Integer
        half = compInv x xinv (k `div` 2)

e727 :: IO ()
e727 = do
    let repack :: forall p n. PolyDivisor p n => Vect Integer -> FinPoly p (Z n)
        repack (Vect y) = mkFinPoly (Poly $ map toZ $ reverse y)

    putText "(i):"
    let x1 = (Vect [7,3,1,0,0])
    -- let xinv1 = (Vect [1,0,1,1,0]) -- given inverse
    let xinv1 = Vect [0,1,1,0,1] -- real inverse (??)
    print $ repack @33 @2 x1 <*> repack xinv1
    print $ repack @33 @2 $ x1 `cProd` xinv1
    let inv1 = compInv x1 xinv1 4
    print $ repack @1048591 @16 x1 <*> repack inv1
    print inv1
    print $ repack @1048591 @16 inv1

    putText "\n(ii):"
    let x2 = (Vect [22,11,5,7,0])
    -- let xinv2 = (Vect [1,0,1,1,0]) -- given
    let xinv2 = (Vect [1,1,0,1,0]) -- real
    print $ repack @33 @2 x2 <*> repack xinv2
    print $ repack @33 @2 $ x2 `cProd` xinv2
    let inv2 = compInv x2 xinv2 7
    print $ repack @34359738495 @128 x2 <*> repack inv2
    print inv2
    print $ repack @34359738495 @128 inv2


    putText "\n(iii):"
    let x3 = (Vect [112,34,239,234,105,180,137])
    --let xinv3 = (Vect [1,0,3,0,5,0,0]) -- given
    let xinv3 = (Vect [2,0,0,1,0,3,0]) -- real
    print $ repack @78129 @5 x3 <*> repack xinv3
    print $ repack @78129 @5 $ x3 `cProd` xinv3
    let inv3 = compInv x3 xinv3 5
    print $ repack @2910383045673370361331249 @3125 x3 <*> repack inv3
    print inv3
    print $ repack @2910383045673370361331249 @3125 inv3


{-

(a) Let f(x)F(x) = np^i, then f(x)G(x) = 1 - n^2 p^(2i), so f(x)G(x) = 1 (mod p^(2i))

(b) Suppose that you know that:
    fG = 1 (mod p^i) = 1 + np^i
    fH = 1 (mod p^j) = 1 + mp^i

    Then f(G+H-fGH) = 1 + np^i + 1 + mp^i - (1 + np^i + mp^i + nmp^(i+j))
                    = 1 + nmp^(i+j)

    So (G+H-fGH) is f's inverse modulo p^(i+j)

    Thus we use double-and-add-like algo to do that: to compute inverse of
    f modulo p^e we do: if e is even, we compute inverse modulo p^(e/2) and
    then combine them using 2 scalar multiplication. If e is odd, we do
    the same, but then also combine inverse mod p^(e-1) with inverse mod p to
    get inverse mod p^e, thus performing 4 scalar multiplications.

    Together it's 4 * log(e) multiplications worst case, 2 * log(e) best case.

(c)

λ> e727
(i):
FinPoly [1]
FinPoly [1]
FinPoly [1]
Vect {unVect = [-18592,-19515,-18777,-19174,-19267]}
FinPoly [13,10,7,5,0]

(ii):
FinPoly [1]
FinPoly [1]
FinPoly [1]
Vect {unVect = [3448250038529,3447348833809,3448339336994,3447637901413,3447781344396]}
FinPoly [12,101,34,17,1]

(iii):
FinPoly [1]
FinPoly [1]
FinPoly [1]
Vect {unVect = [1303487922673017,1303333898736805,1303769823738340,1303106704244461,1303909942885085,1303212428753393,1303729881998585]}
FinPoly [1710,268,710,711,840,2430,1142]

-}

----------------------------------------------------------------------------
-- 7.28
----------------------------------------------------------------------------

{-
(a) First one is quite obvious -- element of ∥b∥^2 (sum of squares)
will have value 1 with probability 2/3 and 0 with 1/3, thus (by the
law of large numbers) the expectated value is 2/3N.

Second:
Every element of the ∥a⋆b∥^2 is a square of c_i, which is sum. It's
a sum of exactly N elements, where index i from (i,j) covers all
elems of 'a' exactly once. E((∑aibi)^2) = ∑a_i*E(bi)

(???)

-}
