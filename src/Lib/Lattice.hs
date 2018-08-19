-- | Functions to work with lattices.

module Lib.Lattice
    (
      lToRat
    , lFromRat
    , express
    , expressInt
    , expressBase
    , expressBaseInt
    , applyBase
    , latticeDet
    , hadamardRatio
    , babaiSolve
    ) where

import Universum

import Control.Lens (_Wrapped)
import qualified Data.List as L

import Lib.Field (Field, Ring (..), fabs)
import Lib.Vector (Matrix (..), Vect (..), determinant, gaussSolveSystem, mFromVecs, mmulm,
                   mtranspose, scal, vdim, vlen, vplus, vtimes)

-- | Converts any Integer-based functor to rational (vector, matrix).
lToRat :: Functor f => f Integer -> f Rational
lToRat = fmap fromInteger

-- | Converts rationals to integers, fails if there's something
-- non-integer inside.
lFromRat :: Functor f => f Rational -> f Integer
lFromRat = fmap (\n -> if denominator n /= 1
                       then error $ "lFromRat: can't convert to int: " <> show n
                       else numerator n)

-- Alternatively, you could do it using adjunct matrix:
--   lFromRat (lToRat x `vmulm` minverse (lToRat $ mFromVecs base))
-- | Express vector in terms of base.
express :: Field f => [Vect f] -> Vect f -> Vect f
express base x = gaussSolveSystem (mtranspose $ mFromVecs base) x

-- | 'express' specified for integers.
expressInt :: [Vect Integer] -> Vect Integer -> Maybe (Vect Integer)
expressInt base x = do
    let res = express (map lToRat base) (lToRat x)
    guard $ all (\n -> denominator n == 1) res
    pure $ lFromRat res

-- | Expresses the second base in the first base. Every ith row of the
-- result is the ith row of the second matrix expressed in the first
-- basis.
expressBase :: Field f => [Vect f] -> [Vect f] -> [Vect f]
expressBase base base' = map (express base) base'

-- | Express base in other base, for integer bases with det = +-1.
expressBaseInt :: [Vect Integer] -> [Vect Integer] -> Maybe [Vect Integer]
expressBaseInt base base' = do
    guard $ detT `elem` [-1,1]
    let res = map lFromRat t
    guard $ (mFromVecs res `mmulm` mFromVecs base) == mFromVecs base'
    pure res
  where
    t :: [Vect Rational]
    t = expressBase (map lToRat base) (map lToRat base')
    detT = determinant $ mFromVecs t

-- | Given a base {bi} and vector k, compute (Sum{i} bi*ki).
applyBase :: [Vect Integer] -> Vect Integer -> Vect Integer
applyBase base coeffs =
    foldl1 vplus $ map (\(b,c) -> c `scal` b) $ base `zip` unVect coeffs

latticeDet :: (Ring a, Ord a) => [Vect a] -> a
latticeDet t = fabs $ determinant $ Matrix $ map unVect t

hadamardRatio :: [Vect Integer] -> Double
hadamardRatio vecs =
    (fromInteger (latticeDet vecs) / product (map vlen vecs)) ** (1/n)
  where
    n = fromIntegral $ vdim $ L.head vecs

-- | Babai's algorithm, returns solution vector and coefficients
-- in the (good) base provided.
babaiSolve :: [Vect Integer] -> Vect Integer -> (Vect Integer, [Integer])
babaiSolve base w = (sol,ks)
  where
    sol :: Vect Integer
    sol = foldl1 vplus $ map (uncurry vtimes) $ ks `zip` base

    -- coefficients
    ks :: [Integer]
    ks =
        map round $
        unVect $
        gaussSolveSystem (mtranspose $ Matrix $ map (unVect . conv) base) (conv w)
      where
        conv :: Vect Integer -> Vect Rational
        conv = _Wrapped %~ map fromInteger
