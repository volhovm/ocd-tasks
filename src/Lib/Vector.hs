{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

-- | Vector arithmetic. TODO make up a better API.

module Lib.Vector
       ( Vect (..)
       , vzero
       , vplus
       , vneg
       , vminus
       , dot
       , scal
       , vlen
       , angle

       , Matrix (..)
       , showMatrix
       , mtranspose
       , gaussSolve
       ) where


import Universum hiding (head, (<*>))

import Control.Lens (ix, (%=), (.=))
import Data.List (head, (!!))
import qualified Data.List as L

import Lib.Field

----------------------------------------------------------------------------
-- Vectors
----------------------------------------------------------------------------

{-
Notice: implementing AGroup for vectors is possible, but it requires lifting
information about vector size to the type (because we should know how many
elements to generate in f0). Instead, I provide a set of ad-hoc methods.
-}

data Vect f = Vect { unVect ::  [f] } deriving (Eq,Ord,Show,Foldable)

vzero :: AGroup f => Integer -> Vect f
vzero n = Vect $ replicate (fromIntegral n) f0

vplus :: AGroup f => Vect f -> Vect f -> Vect f
vplus (Vect a) (Vect b) = Vect $ zipWith (<+>) a b

vneg :: AGroup f => Vect f -> Vect f
vneg (Vect a) = Vect $ map fneg a

vminus :: AGroup f => Vect f -> Vect f -> Vect f
vminus a b = a `vplus` (vneg b)

dot :: Ring f => Vect f -> Vect f -> f
dot (Vect a) (Vect b) = foldl' (<+>) f0 (zipWith (<*>) a b)

scal :: (Ring f) => f -> Vect f -> Vect f
scal k v = Vect $ map (k <*>) $ unVect v

vlen :: Real f => Vect f -> Double
vlen = sqrt . sum . map (sqr . realToFrac) . unVect where sqr x = x * x

angle :: (Ring f, Real f) => Vect f -> Vect f -> Double
angle x y = acos $ realToFrac (dot x y) / (vlen x * vlen y)

----------------------------------------------------------------------------
-- Matrices
----------------------------------------------------------------------------

-- | Row dominated matrix
newtype Matrix a = Matrix { unMatrix :: [[a]] } deriving (Show,Eq)

mtranspose :: Matrix a -> Matrix a
mtranspose = Matrix . L.transpose . unMatrix

-- | Matrix is row-dominated.
showMatrix :: (Show a) => Matrix a -> String
showMatrix (Matrix m) = L.unlines $ map (intercalate " " . map show) m

-- | You pass linear system [A|b], where A is n×n and get list of
-- solutions.
gaussSolve :: forall a. (Field a) => Matrix a -> Matrix a
gaussSolve (Matrix m0)
    | n > m = error "gaussSolve: n > m"
    | otherwise = Matrix $ execState (diagonal1 >> diagonal2) m1
  where
    ix2 :: Int -> Int -> State [[a]] a
    ix2 i j = do (x :: [a]) <- use (ix i)
                 pure $ x !! j

    n = length m0
    m = length $ head m0

    diagonal1 :: State [[a]] ()
    diagonal1 = forM_ [0..(n-1)] $ \(i::Int) -> do
        -- Dividing by diagonal coefficient
        k0 <- ix2 i i
        -- If we're encountered empty row, we swap it with the first
        -- non-zero row. If there is no, we fail.
        k <- if k0 /= f0 then pure k0 else do
                 otherCoeffs <- forM [i+1..(n-1)] $ \j -> (j,) <$> ix2 j i
                 let alt = find (\(_,v) -> v /= f0) otherCoeffs
                 case alt of
                     Nothing -> error "Empty line, can't swap"
                     Just (j,k') -> do
                         rowJ <- use (ix j)
                         rowI <- use (ix i)
                         ix i .= rowI
                         ix j .= rowJ
                         pure k'

        let km1 = finv k
        forM_ [i..(m-1)] $ \j -> (ix i . ix j) %= (<*> km1)

        -- For all lower levels, adding
        forM_ [i+1..(n-1)] $ \j -> do
            s <- ix2 j i
            forM_ [i..m] $ \y -> do
                x <- ix2 i y
                ix j . ix y %= (\e -> e <-> (s <*> x))

    diagonal2 :: State [[a]] ()
    diagonal2 = forM_ (reverse [0..(n-1)]) $ \(i::Int) -> do
        -- For all upper levels, adding
        forM_ [0..i-1] $ \j -> do
            s <- ix2 j i
            forM_ [i..(m-1)] $ \y -> do
                x <- ix2 i y
                ix j . ix y %= (\e -> e <-> (s <*> x))

    initialSort :: [[a]] -> [[a]]
    initialSort = sortBy (comparing $ length . takeWhile (== f0))

    m1 :: [[a]]
    m1 = initialSort m0

----------------------------------------------------------------------------
-- Garbage
----------------------------------------------------------------------------

_testGauss :: IO ()
_testGauss = print $ gaussSolve m
  where
    (m :: Matrix (Z 9539)) =
        Matrix $ map (map toZ)
        [[2,6,1,3030,1],[11,2,0,6892,2],[4,1,3,18312,3]]
