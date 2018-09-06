{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Lattice reduction algorithms.

module Module7v13 () where

import Universum hiding ((<*>))

import qualified Data.List as L

import Lib.Lattice
import Lib.Vector

----------------------------------------------------------------------------
-- 7.43
----------------------------------------------------------------------------

{-

b2* b1 = (b2 - t b1) b1
       = b2 b1 - t (b1 b1)
       = b2 b1 - b1 b2 (b1 b1) / |b1|^2
       = b2 b1 - b1 b2
       = 0

The formula (b2 - t b1) exactly matches the definition of the projection
(7.13.1, page 436). I guess we could calculate the sum of angles, since
we have cos(angle(a,b)) = ab / |a||b| using the cos(α+β) or something
else but it'd be more complicated since we _only_ have cos.

-}

----------------------------------------------------------------------------
-- 7.44
----------------------------------------------------------------------------

{-

(a) |a - t b|^2 is a sum of squares, so we need to minimise |ai - t bi|.

So we have S = sum (ai - t bi)^2

Let's take dS/dt, and see when it's 0:

sum 2 bi (ai - t bi)
  = 2 ( sum (ai bi) - t (sum bi bi) )
  = sum (ai bi) - t (sum bi bi)

Thus t = sum ai bi / sum bi^2 = a * b / |b|^2

(b) |a - ab/|b|^2 b|

(c) It is by the definition.

(d) _draws a picture_

-}

----------------------------------------------------------------------------
-- 7.45 Gauss reduction algorithm
----------------------------------------------------------------------------


e745 :: IO ()
e745 = do
    let v11 = Vect [120670,110521]
    let v12 = Vect [323572,296358]
    print $ gaussReduction v11 v12
    let v21 = Vect [174748650,45604569]
    let v22 = Vect [35462559,9254748]
    print $ gaussReduction v21 v22
    let v31 = Vect [725734520,613807887]
    let v32 = Vect [3433061338,2903596381]
    print $ gaussReduction v31 v32

{-
λ> e745
([Vect {unVect = [14,-47]},Vect {unVect = [-362,-131]}],6)
([Vect {unVect = [147,330]},Vect {unVect = [690,-207]}],7)
([Vect {unVect = [4690,126]},Vect {unVect = [2086,4235]}],11)
-}

----------------------------------------------------------------------------
-- 7.46 TODO
----------------------------------------------------------------------------

{-
(a) W' = ortogonal complement of W ⊂ V.

  Associativity holds naturally, as well as commutativity and inverses.

  Let a, b ∈ W', thus ∀ w ∈ W, a ⊥ w, b ⊥ w
  thus (a + b) w = a w + b w = 0

  0 is in the set, since 0 ⊥ w.

  Also, (cx)w = c(xw) = 0, so cx ∈ W'.

(b) Pick an orthogonal bases consisting of n linearly independent
vectors from W and m from W'. Since all vectors inside W and W' are
linearly independent and for all w ∈ W, w' ∈ W, w ⊥ w', we get an
orthonormal basis of V.

(c) Simply expand |v|^2 = (aw + bw')^2 and apply abww' = 0.

-}

----------------------------------------------------------------------------
-- 7.47
----------------------------------------------------------------------------

e747 :: IO ()
e747 = do
    let (_, vecs) = gaussReduction (Vect [161,120]) (Vect [104,77])
    print vecs
    print $ babaiCVP vecs $ Vect [-9/2, 11]

{-

(b) [Vect {unVect = [-7,2]},Vect {unVect = [3,11]}]
(a) No.
(c) Vect [-4,13],[1,1]

-}

----------------------------------------------------------------------------
-- 7.49
----------------------------------------------------------------------------

e749 :: IO ()
e749 = do
    let shortestVector = maximumBy (comparing snd) . map (\x -> (x, vlen x))
    let m = map Vect
                [ [20, 51, 35, 59, 73, 73]
                , [14, 48, 33, 61, 47, 83]
                , [95, 41, 48, 84, 30, 45]
                , [0, 42, 74, 79, 20, 21]
                , [6, 41, 49, 11, 70, 67]
                , [23, 36, 6, 1, 46, 4]
                ]
    print ("(a)", latticeDet m, hadamardRatio m, shortestVector m)

    let (n1, mReduced) = lllReduction m
    print ("(b)", n1, hadamardRatio mReduced, shortestVector mReduced)

    let (n2, mRevReduced) = lllReduction $ reverse m
    print ("(c)", n2, hadamardRatio mRevReduced, shortestVector mRevReduced)

    let (n3, mOthReduced) = lllReductionRaw 0.99 m
    print ("(c)", n3, hadamardRatio mOthReduced, shortestVector mOthReduced)
{-

λ> e749
("(a)",21242880806,0.45726175341181907,(Vect {unVect = [95,41,48,84,30,45]},151.6278338564526))
("(b)",11,0.9280218742036564,(Vect {unVect = [70,-41,-14,-48,13,-6]},96.36389365317281))
("(c)",8,0.9329873528623579,(Vect {unVect = [39,-42,70,-20,10,-5]},93.32738076256078))
("(c)",12,0.9185610967631723,(Vect {unVect = [36,-69,25,-41,17,43]},102.47438704378767))

-}
