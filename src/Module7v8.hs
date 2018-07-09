{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | GGH PKC

module Module7v8 () where

import Universum

import Control.Lens (ix, _Wrapped)
import Data.List ((!!))
import System.Random (randomRIO)

import Lib.Lattice
import Lib.Misc (suchThat)
import Lib.Vector
import Module7v6 (babaiSolve)

----------------------------------------------------------------------------
-- Common utilites
----------------------------------------------------------------------------

-- Generates matrix U with det = 1.
genU :: Int -> IO (Matrix Integer)
genU n = do
    elems <- replicateM 100 genElem
    let res = foldl mmulm (mId n) elems
    unless (determinant res `elem` [-1,1]) $ error "genU det is not 1 or -1"
    pure res
  where
    uMod :: Matrix Integer -> IO (Matrix Integer)
    uMod m = do
        k <- randomRIO (1,10)
        i <- randomRIO (0,n-1)
        j <- randomRIO (0,n-1) `suchThat` (/= i)
        r <- randomRIO (0::Int,1)
        pure $ case r of
            -- Swap rows
            0 -> let swapRows l = l & ix i .~ (l !! j) & ix j .~ (l !! i)
                 in m & _Wrapped %~ swapRows
            -- Row addition
            _ -> let vs = mToVecs m
                 in mFromVecs $ vs & ix i %~ (`vplus` (k `scal` (vs !! j)))
    -- generate elementary matrix
    genElem :: IO (Matrix Integer)
    genElem = do
        res <- uMod (mId n)
        unless (determinant res `elem` [-1,1]) $ error "genElem det is not 1 or -1"
        pure res

-- Generates lattice base.
genBase :: Int -> IO [Vect Integer]
genBase n = do
    let d = 100
    let genVect = Vect <$> replicateM n (randomRIO (-d, d))
    replicateM n genVect

-- | GGH public key.
data GghPk = GghPk
    { gpBase :: [Vect Integer]
    } deriving Show

-- | GGH secret key.
data GghSk = GghSk
    { gsGoodBase :: [Vect Integer]
    -- Extra fields, these must not be really stored as the secret key
    , gsBadBase  :: [Vect Integer]
    , gsU        :: Matrix Integer
    } deriving Show

validateSk :: GghSk -> Bool
validateSk GghSk{..} = gsBadBase == mToVecs (gsU `mmulm` mFromVecs gsGoodBase)

toPublic :: GghSk -> GghPk
toPublic GghSk{..} = GghPk gsBadBase

-- | Generate SK in a specified dimension.
genSk :: Int -> IO GghSk
genSk n = do
    gsGoodBase <- genGoodBase
    gsU <- genU (length gsGoodBase)
    let gsBadBase = mToVecs $ gsU `mmulm` mFromVecs gsGoodBase
    pure GghSk{..}
  where
    genGoodBase = do
        b <- genBase n
        if hadamardRatio b < 0.5 then genGoodBase else pure b

-- Pass pk, message, and a perturbation vector.
encryptRaw :: GghPk -> [Integer] -> Vect Integer -> Vect Integer
encryptRaw GghPk{..} e r =
    let e' = foldr1 vplus $ map (uncurry scal) $ e `zip` gpBase
    in e' `vplus` r

-- Returns the result and perturbation vector.
decrypt :: GghSk -> Vect Integer -> ([Integer], Vect Integer)
decrypt GghSk{..} e =
    let (closest,_ks) = babaiSolve gsGoodBase e

        msg = lFromRat $ express (map lToRat gsBadBase) (lToRat closest)

        msg' = lFromRat (lToRat closest `vmulm` minverse (lToRat $ mFromVecs gsBadBase))

    in assert "eq" (msg == msg') $ (unVect msg, e `vminus` closest)
  where
    assert t x y = if x then y else error $ "assert: " <> t

----------------------------------------------------------------------------
-- 7.18
----------------------------------------------------------------------------

e718 :: IO ()
e718 = do
    let v1 = Vect [4,13]
    let v2 = Vect [-57, -45]
    let w1 = Vect [25453,9091]
    let w2 = Vect [-16096,-5749]
    let b1 = [v1,v2]
    let b2 = [w1,w2]

    putText $ "Determinant: " <> show (latticeDet b1)
    putText $ "Hadamard good: " <> show (hadamardRatio b1)
    putText $ "Hadamard bad: " <> show (hadamardRatio b2)

    let e = Vect [155340, 55483]

    let u = mFromVecs $
            fromMaybe (error "e718 can't form u") $
            expressBaseInt b1 b2
    let sk = GghSk b1 b2 u
    unless (validateSk sk) $ error "sk is malformed"

    let (ks, r) = decrypt sk e
    putText $ "Decrypted: " <> show ks
    putText $ "Perturbation: " <> show r

    let hole = error "e718 hole"
    putText $ "Decrypted with W: " <> show (decrypt (GghSk b2 b2 hole) e)

{-
Determinant: 561
Hadamard good: 0.7536218249363512
Hadamard bad: 1.1019992284635005e-3
Decrypted: [8,3]
Perturbation: Vect {unVect = [4,2]}
Decrypted with W: ([-8,-23],Vect {unVect = [-11244,-4016]})
-}

----------------------------------------------------------------------------
-- 7.19
----------------------------------------------------------------------------

e719 :: IO ()
e719 = do
    let v = map Vect [[58,53,-68],[-110,-112,35],[-10,-119,123]]
    let w1 = Vect [324850,-1625176,2734951]
    let w2 = Vect [165782,-829409,1395775]
    let w3 = Vect [485054,-2426708,4083804]
    let w = [w1,w2,w3]

    putText $ "Determinant: " <> show (latticeDet w)
    putText $ "Hadamard good: " <> show (hadamardRatio v)
    putText $ "Hadamard bad: " <> show (hadamardRatio w)

    let u = mFromVecs $
            fromMaybe (error "e719 can't form u") $
            expressBaseInt v w
    let sk = GghSk v w u
    unless (validateSk sk) $ error "sk is malformed"

    let e = Vect [8930810,-44681748,75192665]
    let (ks, r) = decrypt sk e
    putText $ "Decrypted: " <> show ks
    putText $ "Perturbation: " <> show r

    let hole = error "e719"
    putText $ "Decrypted with W: " <> show (decrypt (GghSk w w hole) e)

{-
Determinant: 672858
Hadamard good: 0.6169653190266731
Hadamard bad: 2.9999434812456882e-5
Decrypted: [-50,-91,83]
Perturbation: Vect {unVect = [-10,-3,8]}
Decrypted with W: ([52,417,-159],Vect {unVect = [31102,-155615,261874]})
-}

----------------------------------------------------------------------------
-- 7.20
----------------------------------------------------------------------------

{-

(a)
First of all, she can the average: (e + e) / 2 = mW + (r + r') / 2
This is a cyphertext too, and if we suppose that all r are distributed
evenly, and each component is ∈ [-δ,δ], (r + r')/2 has a good chance to
be closer to zero. The smaller it is, the higher the probability
Babai's algorithm will return the correct result.

Also, since perturbation is discrete, whenever we see a e = mW + r,
we know that mW is located somewhere within the hypercube with the side
equal to 2d (at maximum, e is located as its corner). So it's (2d+1)^n
dots.

As we see two points e and e' for the same mW, we know for sure that
space to find mW is reduced to the intersection of cubes created by
e and e'. Intersection volume is calculated as ∏(2d - |x_i-y_i|),
which contains ∏(2d - |x_i-y_i| + 1) dots.

So again, if e and e' are located at the distance of 2d, the volume
of intersection is 0 and thus it is exactly one point -- mW.

Thus the bigger the difference, the smaller the perturbation space to
bruteforce.

(b)
d = 2.

e - e' = (-3,-3,3,-2,1)
|e - e'| = (3,3,3,2,1)

So total number of dots now is (5 - 3)(5 - 3)(5 - 3)(5 - 2)(5 - 1) = 96,
when initially it was 3125.

(c)
If eve correctly guesses the value of m, the recovers r and can then
decode everything sent by Bob deterministically. She can also learn
the m' - m difference. If the algorithm converting plaintexts to elements
of the lattice is specific, it may leak information.

-}

----------------------------------------------------------------------------
-- 7.21
----------------------------------------------------------------------------

{-

(a) Because she can compute e' = m'W + Hash(m').

(b) Since xor is invertible the process of encryption is still
deterministic and also invertible => Alice can decrypt.
If Eve thinks m0 is m0', she can't check that without the knowledge
of r0.

(c) This should be something similar to symmetric encryption. Technically,
the answer to this question is "take some bad symmetric encryption scheme
with a public key", but it'd be slow.

-}
