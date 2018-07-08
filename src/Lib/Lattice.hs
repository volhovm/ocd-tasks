-- | Functions to work with lattices.

module Lib.Lattice
    ( express
    , expressBase
    , expressBaseInt
    , latticeDet
    , hadamardRatio
    ) where

import Universum

import Control.Lens (each, _Wrapped)
import qualified Data.List as L

import Lib.Field (Field, Ring (..), fabs)
import Lib.Vector (Matrix (..), Vect (..), determinant, gaussSolveSystem, mFromVecs, mmulm,
                   mtranspose, vdim, vlen)

express :: Field f => [Vect f] -> Vect f -> Vect f
express base x = gaussSolveSystem (mtranspose $ mFromVecs base) x

-- Expresses the second base in the first base. Every ith row of the
-- result is the ith row of the second matrix expressed in the first
-- basis.
expressBase :: Field f => [Vect f] -> [Vect f] -> [Vect f]
expressBase base base' = map (express base) base'

expressBaseInt :: [Vect Integer] -> [Vect Integer] -> Maybe [Vect Integer]
expressBaseInt base base' = do
    guard $ detT `elem` [-1,1]
    let res = map (fmap (\n -> if denominator n /= 1
                               then error "expressBaseInt"
                               else numerator n))
                  t
    guard $ (mFromVecs res `mmulm` mFromVecs base) == mFromVecs base'
    pure res
  where
    conv = map (_Wrapped . each %~ fromInteger)
    t :: [Vect Rational]
    t = expressBase (conv base) (conv base')
    detT = determinant $ mFromVecs t

latticeDet :: (Ring a, Ord a) => [Vect a] -> a
latticeDet t = fabs $ determinant $ Matrix $ map unVect t

hadamardRatio :: [Vect Integer] -> Double
hadamardRatio vecs =
    (fromInteger (latticeDet vecs) / product (map vlen vecs)) ** (1/n)
  where
    n = fromIntegral $ vdim $ L.head vecs
