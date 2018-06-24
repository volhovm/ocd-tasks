{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Lattices: basic properties.

module Module7v4 () where

import Universum

import Data.List ((!!))

import Lib.Lattice
import Lib.Vector

----------------------------------------------------------------------------
-- 7.7
----------------------------------------------------------------------------

{-
Not going to draw anything.
-}

e77 :: IO ()
e77 = do
    print $ determinant (Matrix [[1,3,-2],[2,1,0],[-1,2,5]] :: Matrix Double)

{-
λ> e77
-35.0
-}

----------------------------------------------------------------------------
-- 7.8
----------------------------------------------------------------------------

{-
Let's assume that there's some v such that it has other w in any neighborhood
of radius e. Then, since L is closed under addition/subtraction, w-v ∈ L
and it's the small vector that is pointing from w to v. But ∥w-v∥ < ε, so it
means this vector is also in close neighborhood of 0, which is false by
assumption.
-}

----------------------------------------------------------------------------
-- 7.9
----------------------------------------------------------------------------

{-
Contraposition. So if L is not closed under addition, it's obviously not
lattice. If L is continuous somewhere, every element should be expressed
as integer combination of base vectors, but this continuity is not
countable, which brings us to the situation some elemnents from it won't
be expressable, since L is not a lattice.
-}

----------------------------------------------------------------------------
-- 7.10
----------------------------------------------------------------------------

{-
(a) Purely mechanical check.

    Let S = AB and C is a cofactor matrix (transpose of B).

    Diagonal elements of S are scalar multiplication of row i of A
    and row i of C, which is a Laplace expansion formula. Thus, (i,i)
    elements are det(A).

    Non-diagonal elements (i,j) are zero: it's a scalar multiplication
    of row i of A ond row j of C. Since cofactors in row j don't carry
    any information about elements of row i of A, we can interpret
    this scalar multiplication as determinant of A', where A' is
    A where values in row j are replaced with values from row i. This
    matrix determinant is obviously zero.

(b) Trivially follows from (a)

(c) 1/det(A) must exist, but in the integer ring the only value that
    divides 1 is (+-1).

(d) Again, trivially follows -- in order for x = det(A) to be invertible,
    it must be unit, that's how ring units are defined.
-}

----------------------------------------------------------------------------
-- 7.11
----------------------------------------------------------------------------

{-

(a),(b) Determinant preserves multiplication. Also det(A^{ -1 }) = 1/det(A),
  which doesn't bring the value out of { -1,1}.
(c) Determinant of I is 1.
(d) Associativity follows from the associativity of matrix multiplication
  in R.
(e) No, it's not commutative. It's easy to show a counterexample.

-}

----------------------------------------------------------------------------
-- 7.12
----------------------------------------------------------------------------

e712 :: IO ()
e712 = do
    let solve x = do
            let a = adjunct x
            let d = determinant x
            print a
            print d
            print $ a `mmulm` x
            let xinv = d `mscal` a
            print $ xinv
            print $ x `mmulm` xinv
            putText ""

    let a = Matrix [[3,1],[2,2]] :: Matrix Integer
    let b = Matrix [[3,-2],[2,-1]] :: Matrix Integer
    let c = Matrix [[3,2,2],[2,1,2],[-1,3,1]] :: Matrix Integer
    let d = Matrix [[-3,-1,2],[1,-3,-1],[3,0,-2]] :: Matrix Integer

    forM_ [a,b,c,d] solve

{-
λ> e712
Matrix {unMatrix = [[2,-1],[-2,3]]}
4
Matrix {unMatrix = [[4,0],[0,4]]}
Matrix {unMatrix = [[8,-4],[-8,12]]}
Matrix {unMatrix = [[16,0],[0,16]]}

Matrix {unMatrix = [[-1,2],[-2,3]]}
1
Matrix {unMatrix = [[1,0],[0,1]]}
Matrix {unMatrix = [[-1,2],[-2,3]]}
Matrix {unMatrix = [[1,0],[0,1]]}

Matrix {unMatrix = [[-5,4,2],[-4,5,-2],[7,-11,-1]]}
-9
Matrix {unMatrix = [[-9,0,0],[0,-9,0],[0,0,-9]]}
Matrix {unMatrix = [[45,-36,-18],[36,-45,18],[-63,99,9]]}
Matrix {unMatrix = [[81,0,0],[0,81,0],[0,0,81]]}

Matrix {unMatrix = [[6,-2,7],[-1,0,-1],[9,-3,10]]}
1
Matrix {unMatrix = [[1,0,0],[0,1,0],[0,0,1]]}
Matrix {unMatrix = [[6,-2,7],[-1,0,-1],[9,-3,10]]}
Matrix {unMatrix = [[1,0,0],[0,1,0],[0,0,1]]}
-}

----------------------------------------------------------------------------
-- 7.13
----------------------------------------------------------------------------

e713 :: IO ()
e713 = do
    let base = map Vect [[3,1,-2],[1,-3,5],[4,2,1]] :: [Vect Rational]
    let base1 = map Vect [[5,13,-13],[0,-4,2],[-7,-13,18]] :: [Vect Rational]
    let base2 = map Vect [[4,-2,3],[6,6,-6],[-2,-4,7]] :: [Vect Rational]
    let testBase base' = do
            let t = expressBase base base'
            print $ latticeDet t
    testBase base1
    testBase base2

{-
First one has det 1 (ok), second -- -2.
λ> e713
1 % 1
(-2) % 1
-}

----------------------------------------------------------------------------
-- 7.14
----------------------------------------------------------------------------

e714 :: IO ()
e714 = do
    let b = [[1,0,1,-1],[1,2,0,4],[1,-1,2,1]] :: [[Double]]
    let gram x =
            let n = length x - 1
                foo :: Int -> Int -> Double
                foo i j = (Vect $ x !! i) `dot` (Vect $ x !! j)
            in Matrix $ map (\i -> map (foo i) [0..n]) [0..n]
    let detGram = sqrt . determinant . gram
    print $ gram b
    print $ detGram b

{-
(a) Trivial.

(b) Since G = F * F^T, det(G) = det(F) * det(F^T). det(L) = |det(F)|, so
    det(G) = det(L)^2

(c)
λ> e714
Matrix {unMatrix = [[3.0,-3.0,2.0],[-3.0,21.0,3.0],[2.0,3.0,7.0]]}
15.198684153570664

(d) Obviously, since det(L)^2 is exactly ∥v1*∥^2 * ...
-}
