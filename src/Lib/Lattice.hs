-- | Functions to work with lattices.

module Lib.Lattice
    (
      lToRat
    , lFromRat
    , express
    , expressBase
    , expressBaseInt
    , latticeDet
    , hadamardRatio
    ) where

import Universum

import qualified Data.List as L

import Lib.Field (Field, Ring (..), fabs)
import Lib.Vector (Matrix (..), Vect (..), determinant, gaussSolveSystem, mFromVecs, mmulm,
                   mtranspose, vdim, vlen)

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

latticeDet :: (Ring a, Ord a) => [Vect a] -> a
latticeDet t = fabs $ determinant $ Matrix $ map unVect t

hadamardRatio :: [Vect Integer] -> Double
hadamardRatio vecs =
    (fromInteger (latticeDet vecs) / product (map vlen vecs)) ** (1/n)
  where
    n = fromIntegral $ vdim $ L.head vecs
