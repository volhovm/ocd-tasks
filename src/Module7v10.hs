{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | NTRU PKC.

module Module7v10 () where

import Universum hiding ((<*>))

import Data.Reflection (reifyNat)

import Lib.Field
import Lib.Vector

data NtruParams = NtruParams
    { ntruN :: Integer
    , ntruP :: Integer
    , ntruQ :: Integer
    , ntruD :: Integer
    } deriving (Show)

data NtruSk = NtruSk
    { nsF     :: Vect Integer  -- In R
    , nsG     :: Vect Integer  -- In R
    , nsFinvP :: Vect Integer -- In R_p
    , nsFinvQ :: Vect Integer -- In R_q
    } deriving (Show)

data NtruPk = NtruPk
    { npH :: Vect Integer -- In R_q
    } deriving (Show)

data NtruMsg = NtruMsg (Vect Integer) deriving Show

centerLift :: forall q. KnownNat q => Vect (Z q) -> Vect Integer
centerLift x =
    let q' = natValI @q
    in map (\e -> if e > q' `div` 2 then e-q' else e) $ map unZ x

toPublic :: NtruParams -> NtruSk -> NtruPk
toPublic NtruParams{..} NtruSk{..} = NtruPk pk
  where
    pk = reifyNat ntruQ $ \(_ :: Proxy q) ->
         centerLift $
         (map (toZ @q) nsFinvQ) `cProd` (map (toZ @q) nsG)

ntruEncrypt :: NtruParams -> NtruPk -> Vect Integer -> Vect Integer -> NtruMsg
ntruEncrypt NtruParams{..} NtruPk{..} m r =
    reifyNat ntruQ $ \(_ :: Proxy q) ->
        let conv :: Vect Integer -> Vect (Z q)
            conv = map (toZ @q)
            e = ((toZ @q ntruP) `scal` (conv npH `cProd` conv r)) `vplus` conv m
        in NtruMsg $ centerLift e

ntruDecrypt :: NtruParams -> NtruSk -> NtruMsg -> Vect Integer
ntruDecrypt NtruParams{..} NtruSk{..} (NtruMsg e) =
    let a = reifyNat ntruQ $ \(_ :: Proxy q) ->
            centerLift $ map (toZ @q) nsF `cProd` map (toZ @q) e
        b = reifyNat ntruP $ \(_ :: Proxy p) ->
            centerLift $ map (toZ @p) nsFinvP `cProd` map (toZ @p) a
    in b

polyToVect :: Poly a -> Vect a
polyToVect = Vect . reverse . unPoly

vectToPoly :: AGroup a => Vect a -> Poly a
vectToPoly = stripZ . Poly . reverse . unVect

ntruTest :: IO ()
ntruTest = do
    let params = NtruParams 7 3 41 2
    let f = Vect [-1, 0, 1, 1, -1, 0, 1]
    let g = Vect [0, -1, -1, 0, 1, 0, 1]

    let fq = FinPoly (Poly $ reverse $ unVect $ map (toZ @41) f) :: FinPolyZ 194754273921 41
    let fp = FinPoly (Poly $ reverse $ unVect $ map (toZ @3) f) :: FinPolyZ 2189 3

    print fq
    print fp

    let Just _Fp = invPolyEuclidian fp
    print _Fp
    let Just _Fq = invPolyEuclidian fq
    print _Fq

    let sk = NtruSk f g (centerLift $ polyToVect $ unFinPoly _Fp)
                        (centerLift $ polyToVect $ unFinPoly _Fq)
    let pk = toPublic params sk
    print pk

    let m = Vect [1,-1,1,1,0,-1,0]
    let r = Vect [-1,1,0,0,0,-1,1]
    let e@(NtruMsg e') = ntruEncrypt params pk m r
    print $ map (toZ @41) e'

    print $ ntruDecrypt params sk e
    print $ ntruDecrypt params sk e == m
