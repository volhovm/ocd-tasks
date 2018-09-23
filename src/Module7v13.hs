{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Lattice reduction algorithms.

module Module7v13 (e750) where

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
-- 7.48
----------------------------------------------------------------------------

-- > by hand
-- what do I program with, you think?
e748 :: IO ()
e748 = print $ lllReduction [Vect [20,16,3], Vect [15,0,10], Vect [0,18,9]]

-- λ> e748
-- (1,[Vect [15,0,10],Vect [5,16,-7],Vect [-5,2,16]])

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
    print ("(a)"::Text, latticeDet m, hadamardRatio m, shortestVector m)

    let (n1, mReduced) = lllReduction m
    print ("(b)"::Text, n1, hadamardRatio mReduced, shortestVector mReduced)

    let (n2, mRevReduced) = lllReduction $ reverse m
    print ("(c)"::Text, n2, hadamardRatio mRevReduced, shortestVector mRevReduced)

    let (n3, mOthReduced) = lllReductionRaw 0.99 m
    print ("(c)"::Text, n3, hadamardRatio mOthReduced, shortestVector mOthReduced)
{-

λ> e749
("(a)",21242880806,0.45726175341181907,(Vect {unVect = [95,41,48,84,30,45]},151.6278338564526))
("(b)",11,0.9280218742036564,(Vect {unVect = [70,-41,-14,-48,13,-6]},96.36389365317281))
("(c)",8,0.9329873528623579,(Vect {unVect = [39,-42,70,-20,10,-5]},93.32738076256078))
("(c)",12,0.9185610967631723,(Vect {unVect = [36,-69,25,-41,17,43]},102.47438704378767))

-}

----------------------------------------------------------------------------
-- 7.50
----------------------------------------------------------------------------

{-

(a) Provided algorithm is a more "efficient" version of the original one,
and it's completely isomorphic to it. The only huge difference is how
it handles values (more efficiently). On the other hand, it's terrible
-- goto antipatterns, generally a spaghetti.

So main routine consists of the initialisation part (1-2), new k intoduction
part (3-8) which computes new μ_{k,j} and vk*, and main reduction loop
9-18. The loop first reduces kth vector and performs a series of swaps
(l9-12), until all adjacent pairs in {v1..vk} satisfy Lovasz condition. Then
it reduces all other vecs k-2..1.

vk* values are computed and used once to fill up Bk. These Bk are used instead
in the Lovasz condition check and to compute μ in line 6, so we do not really
need vk* by themselves.

RED(k,l) sets vk = vk - (round μ_{k,j}) vl, but updates μs
manually. SWAP does the swap step, also recomputing μs and Bs affected.

Overall, I won't reimplement my neat functional (slow) version of LLL.

(b) Sadly, determinant computation takes a lot of time even for small N,
so here's a result for (i) only. :(

λ> e750
(i)
Hadamard: (0.13584606696098708,0.8358661155734323)
Gauss: 82.43578055382737
Shortest: 88.39683252243827

-}

e750 :: IO ()
e750 = do
    let solve a n q = do
            let makeV i = Vect [ ((i + n) ^ j) `mod` q | j <- [0..n-1]]
                base = map makeV [0..n-1]
            let base' = snd $ lllReduction base
            putText a
            putText $ "Hadamard: " <> show (hadamardRatio base, hadamardRatio base')
            putText $ "Gauss: " <> show (gaussianShortest base')
            putText $ "Shortest: " <> show (vlen $ minimumBy (comparing vlen) base')
    solve "(i)" 10 541
    -- solve "(ii)" 20 863

----------------------------------------------------------------------------
-- 7.51
----------------------------------------------------------------------------

{-

(a) β = (α - 1/4) is the coefficient:

    |vj*|^2 ≤ β^{i-j}|vi*|^2

    let ci = (β - β^i)/(4 (1 - β)) + 1

    |vi|^2 ≤ ci |vi*|^2

    prod |vi|^2 ≤ prod ci |vi*|^2 =

(b) Instead of fixed 3/4 factor, we'll have

    d_{k-1}^new ≤ β * d_{k-1}^{old}

    1 ≤ d_fin ≤ β^N d_init

    And then we perform the same steps -- asymptotically there is no
    difference between these cases.

    But N >= (log dinit - log 1) / log β in our case, so the less
    β is, the bigger is the constant.

-}

----------------------------------------------------------------------------
-- 7.52 TODO
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- 7.53
----------------------------------------------------------------------------

babaiClosestPlane :: [Vect Integer] -> Vect Integer -> Vect Integer
babaiClosestPlane base t = t `vminus` endw
  where
    base' = gramSchmidt $ map lFromInt base
    loop w (-1) = w
    loop w i =
        let vi = base L.!! i
            vi' = base' L.!! i
            k :: Integer
            k = round $ (lFromInt w `dot` vi') / (vlen vi' ^ (2 :: Int))
        in loop (w `vminus` (k `scal` vi)) (i-1)
    endw :: Vect Integer
    endw = loop t (fromIntegral (vdim t) - 1)

e753 :: IO ()
e753 = do
    let solve m t = do
            let s1 = fst $ babaiCVP m (lFromInt t)
            let s2 = babaiClosestPlane m t
            putText $ "Babai: " <> show (vlen (t `vminus` s1))
            putText $ "Babai closest plane: " <> show (vlen (t `vminus` s2))

    let m1 =
          map Vect $
          [[-5, 16, 25, 25, 13, 8]
          ,[26, -3, -11, 14, 5, -26]
          ,[15, -28, 16, -7, -21, -4]
          ,[32, -3, 7, -30, -6, 26]
          ,[15, -32, -17, 32, -3, 11]
          ,[5, 24, 0, -13, -46, 15]
          ]
    let t1 = Vect [-178,117,-407,419,-4,252]
    putText "(a)"
    solve m1 t1

    let m2 = map Vect $
          [[-33, -15, 22, -34, -32, 41]
          ,[10, 9, 45, 10, -6, -3]
          ,[-32, -17, 43, 37, 29, -30]
          ,[26, 13, -35, -41, 42, -15]
          ,[-50, 32, 18, 35, 48, 45]
          ,[2, -5, -2, -38, 38, 41]]
    let t2 = Vect [-126,-377, -196, 455, -200, -234]
    putText "(b)"
    solve m2 t2

    let m3 = snd $ lllReduction m2
    putText "(c)"
    solve m3 t2

{-

λ> e753
(a)
Babai: 33.28663395418648
Babai closest plane: 29.715315916207253
(b)
Babai: 58.01723881744115
Babai closest plane: 53.65631370118525
(c)
Babai: 51.0
Babai closest plane: 33.91164991562634

-}
