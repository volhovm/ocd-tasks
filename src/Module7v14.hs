{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | LLL application.

module Module7v14 () where

import Universum hiding ((<*>))

import qualified Data.List as L

import Lib.Field
import Lib.Lattice
import Lib.Vector
import Module7v1 as M1
import Module7v10 as M10
import Module7v8 as M8

----------------------------------------------------------------------------
-- 7.55 Congruential cryptosystem
----------------------------------------------------------------------------

-- 7.14.1 illustrated
e755_test :: IO ()
e755_test = do
    let q = 122430513841
    let h = 39245579300
    let v1 = Vect [1, h]
    let v2 = Vect [0, q]
    let base' = snd $ gaussReduction v1 v2
    let shortest = minimumBy (comparing vlen) base'

    print $ base'
    print $ gaussianShortest base'
    print $ shortest

e755 :: IO ()
e755 = do
    let e = CgM 83493429501
    let h = 24201896593
    let q = 148059109201
    let v1 = Vect [1, h]
    let v2 = Vect [0, q]
    let base' = snd $ gaussReduction v1 v2
    let shortest = minimumBy (comparing vlen) base'

    print $ base'
    print $ gaussianShortest base'
    print $ shortest

    let Vect [f,g] = shortest
    let sk = CgSk q f g

    print $ cgDec sk e

{-
λ> e755
[Vect [233444,255333],Vect [330721,-272507]]
131672.59319508614
Vect [233444,255333]
186000
-}


----------------------------------------------------------------------------
-- 7.56 Subset-sum
----------------------------------------------------------------------------


e756_solve :: [Integer] -> Integer -> IO ()
e756_solve m s = do
    let n = length m
    let base =
            map Vect $
            transpose ((++[m]) $ transpose $ unMatrix (2 `mscal` mId n)) ++
            [replicate n 1 ++ [s]]
    let base' = snd $ lllReduction base
    let Just (Vect sol) =
            find (\(Vect x) -> L.last x == 0 && all (`elem` [-1,1]) (take n x)) base'
    print sol
    let transform :: Integer -> Integer
        transform 0 = 1
        transform 1 = 0
        transform x = x
    let k = map transform sol
    print k
    print $ sum $ map (uncurry (*)) $ k `zip` (m ++ [s])

e756_test :: IO ()
e756_test = e756_solve [89, 243, 212, 150, 245] 546

e756 :: IO ()
e756 = e756_solve m s
  where
     m = [81946,80956,58407,51650,38136,17032,39658,67468,49203,9546]
     s = 168296

{-
λ> e756
[1,-1,1,1,-1,1,-1,1,1,-1,0]
[0,-1,0,0,-1,0,-1,0,0,-1,1]
0
-}

----------------------------------------------------------------------------
-- 7.57 GGH
----------------------------------------------------------------------------

e757_solve :: [Vect Integer] -> Vect Integer -> IO ()
e757_solve base e = do
    let hole = error "hole"
    let base' = snd $ lllReduction base
    print $ M8.decrypt (M8.GghSk base' base hole) e

e757_test :: IO ()
e757_test = e757_solve m e
  where
    m = map Vect $
           [ [-4179163, -1882253, 583183]
           , [-3184353, -1434201, 444361]
           , [-5277320, -2376852, 736426]]
    e = Vect [-79081427,-35617462,11035473]

e757 :: IO ()
e757 = e757_solve m e
  where
    m = map Vect $
           [ [10305608, -597165, 45361210, 39600006, 12036060]
           , [-71672908, 4156981, -315467761, -275401230, -83709146]
           , [-46304904, 2685749, -203811282, -177925680, -54081387]
           , [-68449642, 3969419, -301282167, -263017213, -79944525]
           , [-46169690, 2677840, -203215644, -177405867, -53923216]]
    e = Vect [388120266,-22516188,1708295783,1491331246,453299858]

{-
λ> e757_test
([86,-35,-32],Vect [-4,-3,2])
λ> e757
([-3,-9,0,6,-4],Vect [10,-8,-10,4,10])
-}

----------------------------------------------------------------------------
-- 7.58 NTRU
----------------------------------------------------------------------------

e758_solve :: Integer -> Vect Integer -> IO (Vect Integer, Vect Integer)
e758_solve q h = do
    let n = fromInteger $ vdim h
    let mH0 = map (take n) $ take n $ tails $ cycle $ unVect h
    let mH = L.head mH0 : reverse (L.tail mH0)
    let matrix =
            mToVecs $
            (mId n `mConcH` Matrix mH) `mConcV` (mNull n `mConcH` (q `mscal` mId n))
    let matrix' = snd $ lllReduction matrix
    let shortest = minimumBy (comparing vlen) matrix'
    pure $ bimap Vect Vect $ L.splitAt n (unVect shortest)

e758_test :: IO ()
e758_test = print =<< e758_solve 41 (Vect [30, 26, 8, 38, 2, 40, 20])

e758 :: IO ()
e758 = do
    (f,g) <- e758_solve 67 (Vect [39,9,33,52,58,11,38,6,1,48,41])

    let fq = FinPoly (Poly $ reverse $ unVect $ map (toZ @67) f) :: FinPolyZ 122130132904968017149 67
    let fp = FinPoly (Poly $ reverse $ unVect $ map (toZ @3) f) :: FinPolyZ 177149 3

    let Just fpInv = invPolyEuclidian fp
    let Just fqInv = invPolyEuclidian fq

    let sk = M10.NtruSk f g (centerLift $ polyToVect $ unFinPoly fpInv)
                            (centerLift $ polyToVect $ unFinPoly fqInv)

    let params = M10.NtruParams 11 3 67 3

    let e = M10.NtruMsg $ Vect [52,50,50,61,61,7,53,46,24,17,50]
    print $ M10.ntruDecrypt params sk e

{-

λ> e758_test
(Vect [1,0,-1,1,0,-1,-1],Vect [-1,0,-1,0,1,1,0])

λ> e758
Vect [1,-1,-1,-1,-1,0,0,1,0,0,1]

-}
