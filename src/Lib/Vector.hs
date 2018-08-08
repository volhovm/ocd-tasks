{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TypeFamilies   #-}

-- | Vector arithmetic. TODO make up a better API.

module Lib.Vector
       ( Vect (..)
       , vzero
       , vplus
       , vneg
       , vminus
       , vtimes
       , dot
       , scal
       , vlen
       , vdim
       , angle
       , cProd

       , Matrix (..)
       , mToVecs
       , mFromVecs
       , showMatrix
       , mNull
       , mId
       , msize
       , mtranspose
       , maddm
       , mscal
       , vmulm
       , mmulm
       , minor
       , determinant
       , cofactor
       , adjunct
       , minverse
       , gaussSolve
       , gaussSolveSystem
       ) where


import Universum hiding (head, (<*>))

import Control.Lens (each, ix, makeWrapped, (%=), (.=), _Wrapped)
import Data.List (head, (!!))
import qualified Data.List as L
import GHC.Exts (IsList (..))

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
$(makeWrapped ''Vect)

instance IsList (Vect f) where
    type Item (Vect f) = f
    toList = unVect
    fromList = Vect

instance Functor Vect where
    fmap func v = v & _Wrapped %~ fmap func

vzero :: AGroup f => Integer -> Vect f
vzero n = Vect $ replicate (fromIntegral n) f0

vplus :: AGroup f => Vect f -> Vect f -> Vect f
vplus (Vect a) (Vect b) = Vect $ zipWith (<+>) a b

vneg :: AGroup f => Vect f -> Vect f
vneg (Vect a) = Vect $ map fneg a

vminus :: AGroup f => Vect f -> Vect f -> Vect f
vminus a b = a `vplus` (vneg b)

vtimes :: AGroup f => Integer -> Vect f -> Vect f
vtimes n v = fastTimes z vneg vplus n v
  where
    z = vzero (fromIntegral $ length v)

dot :: Ring f => Vect f -> Vect f -> f
dot (Vect a) (Vect b) = foldl' (<+>) f0 (zipWith (<*>) a b)

scal :: (Ring f) => f -> Vect f -> Vect f
scal k v = Vect $ map (k <*>) $ unVect v

-- | Euclidian distance.
vlen :: Real f => Vect f -> Double
vlen = sqrt . sum . map (sqr . realToFrac) . unVect where sqr x = x * x

-- | Vector dimension
vdim :: Vect f -> Integer
vdim = fromIntegral . length . unVect

angle :: (Ring f, Real f) => Vect f -> Vect f -> Double
angle x y = acos $ realToFrac (dot x y) / (vlen x * vlen y)

-- Convolution product
cProd :: Ring f => Vect f -> Vect f -> Vect f
cProd (Vect a) (Vect b)
    | length a /= length b = error "cProd sizes"
    | otherwise = Vect $ map ck l
  where
    n = length a
    l = [0..n-1]
    inRange x = x >= 0 && x < n
    ck k =
        foldr1 (<+>) $
        map (\(i,j) -> a !! i <*> b !! j)
            [(i,j) | i <- l, j <- l, inRange i, inRange j, (i + j) `mod` n == k]

----------------------------------------------------------------------------
-- Matrices
----------------------------------------------------------------------------

-- | Row dominated 2d matrix.
newtype Matrix a = Matrix { unMatrix :: [[a]] } deriving (Show,Eq)
$(makeWrapped ''Matrix)

instance Functor Matrix where
    fmap func v = v & _Wrapped . each . each %~ func

-- | Represent list of vectors as rows of matrix.
mFromVecs :: [Vect a] -> Matrix a
mFromVecs = Matrix . map unVect

-- | Convert matrix to list of rows.
mToVecs :: Matrix a -> [Vect a]
mToVecs = map Vect . unMatrix

-- | Matrix is row-dominated.
showMatrix :: (Show a) => Matrix a -> String
showMatrix (Matrix m) = L.unlines $ map (intercalate " " . map show) m

-- | Zero matrix.
mNull :: Ring a => Int -> Matrix a
mNull n = Matrix $ replicate n (replicate n f0)

-- | Identity matrix.
mId :: Ring a => Int -> Matrix a
mId n = Matrix $ map (\i -> replicate i f0 ++ [f1] ++ replicate (n-i-1) f0) [0..n-1]

-- | Matrix (n,m) size.
msize :: Matrix a -> (Int,Int)
msize (Matrix l) = (length l, length (head l))

-- | Matrix transpose.
mtranspose :: Matrix a -> Matrix a
mtranspose = Matrix . L.transpose . unMatrix

-- | Matrix addition
maddm :: AGroup a => Matrix a -> Matrix a -> Matrix a
maddm m1 m2 =
    if msize m1 /= msize m2
    then error $ "maddm dimensions must be equal: " <> show (msize m1, msize m2)
    else mFromVecs $ map (uncurry vplus) $ mToVecs m1 `zip` mToVecs m2

-- | Scalar matrix multiplication
mscal :: Ring a => a -> Matrix a -> Matrix a
mscal k (Matrix m) = Matrix $ map (\r -> map (<*> k) r) m

-- | Multiply (horisontal) vector by matrix.
vmulm :: Ring a => Vect a -> Matrix a -> Vect a
vmulm (Vect v) m0@(Matrix m)
    | not sizesMatch = error "vmulm wrong sizes"
    | otherwise = Vect $ map (product' v) (transpose m)
  where
    sizesMatch = length v == fst (msize m0)
    product' a b = foldl' (<+>) f0 (zipWith (<*>) a b)

-- | Matrix multiplication
mmulm :: Ring a => Matrix a -> Matrix a -> Matrix a
mmulm m1 m2 =
    if snd (msize m1) /= fst (msize m2)
    then error $ "mmulm dimensions: " <> show (msize m1, msize m2)
    else m1 & _Wrapped %~ map (\v -> unVect $ (Vect v) `vmulm` m2)

-- | Matrix minor.
minor :: Matrix a -> Int -> Int -> Matrix a
minor (Matrix rows) i j = Matrix $ map (dropAround j) $ dropAround i rows
  where
    dropAround 0 l = drop 1 l
    dropAround k l | k == length l - 1 = take (length l - 1) l
    dropAround k l = take k l <> drop (k+1) l

-- | Matrix's determinant. Works only for square matrices.
determinant :: Ring a => Matrix a -> a
determinant (Matrix s) | length s /= length (head s) = error "determinant: matrix is not square"
determinant (Matrix [[x]]) = x
determinant m@(Matrix rows) =
    foldr1 (<+>) $
    map (\((e,k),i) -> k <*> e <*> determinant (minor m 0 i))
        ((head rows `zip` cycle [f1,fneg f1]) `zip` [0..])

-- | Cofactor matrix.
cofactor :: Ring a => Matrix a -> Matrix a
cofactor m@(Matrix rows) =
    Matrix $
    map (\i -> map (\j -> ij i j <*> determinant (minor m i j))
                   [0 .. (length (head rows) - 1)])
        [0 .. (length rows - 1)]
  where
    ij i j | even (i + j) = f1
           | otherwise = fneg f1

-- | Adjunct matrix.
adjunct :: Ring a => Matrix a -> Matrix a
adjunct x = let res = mtranspose $ cofactor x
            in if x `mmulm` res == determinant x `mscal` (mId (fst $ msize x))
               then res
               else error "adjunct"

-- | Matrix inverse.
minverse :: forall a. (Field a, Show a) => Matrix a -> Matrix a
minverse m | n /= k = error "minverse wrong input size"
           | not valid = error "minverse failed" -- do you use doubles?
           | otherwise = res
  where
    valid = res `mmulm` m == mId n
    res = (finv (determinant m)) `mscal` adjunct m
    (n,k) = msize m

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

-- | v * X = y, solves for v
gaussSolveSystem :: forall a. (Field a) => Matrix a -> Vect a -> Vect a
gaussSolveSystem m (unVect -> x) =
    case () of
      () | length res /= length (head (unMatrix m)) -> error "gaussSolveSystem: dimensions"
         | not check -> error "gaussSolveSystem: failed"
         | otherwise -> Vect res
  where
    m' = Matrix $ map (\(r,v) -> r ++ [v]) $ unMatrix m `zip` x
    res = map L.last $ unMatrix $ gaussSolve m'
    check = all (\(r,v) -> foldr1 (<+>) (map (uncurry (<*>)) $ r `zip` res) == v)
                (unMatrix m `zip` x)

----------------------------------------------------------------------------
-- More vectors (because TH doesn't let me define it in arbitrary order)
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- Garbage
----------------------------------------------------------------------------

_testGauss :: IO ()
_testGauss = do
    let s1 = gaussSolveSystem m $ fromList [3030,6892,18312] -- this doesn't
    let s2 = gaussSolveSystem m $ fromList [2,2,3] -- this works
    print s1
    print s2
  where
    (m :: Matrix (Z 9539)) =
        Matrix $ map (map toZ) [[2,6,1],[11,2,0],[4,1,3]]
