{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Pollard's ρ method.

module Module5v5 (pollardRho) where

import Universum hiding (exp)

import Lib (exEucl, exp, inverseP)

----------------------------------------------------------------------------
-- 5.40
----------------------------------------------------------------------------

data PollardAcc = PollardAcc
    { paA :: !Integer
    , paB :: !Integer
    , paG :: !Integer
    , paD :: !Integer
    } deriving Show

pollardAfter :: PollardAcc -> Integer -> Integer -> Integer -> Integer
pollardAfter PollardAcc{..} p g h =
    case exEucl v (p-1) of
        (1,_,_) -> u * (inverseP p v)
        (d,s,_) -> let w = (u * s) `mod` (p-1)
                       r = (p-1) `div` d
                   in fromMaybe (error "pollardAfter") $
                      find (\e -> exp p g e == h) $
                      map (\k -> w `div` d + k * r) [0..d-1]
  where
    u = (paA - paG) `mod` (p-1)
    v = (paD - paB) `mod` (p-1)

{-
λ> pollardAfter (PollardAcc 81756 9527 67782 28637) 81799 11 41387
64857
λ> logDShank 81799 11 41387
64857

Must be correct.
-}

----------------------------------------------------------------------------
-- 5.41
----------------------------------------------------------------------------

pollardRho :: Integer -> Integer -> Integer -> Integer
pollardRho p g h = go 1 1 $ PollardAcc 0 0 0 0
  where
    go x y acc | x == y = pollardAfter acc p g h
    go x y PollardAcc{..} = do
        let (x',a',b') = pFoo x paA paB
        let (y',g',d') =
                let (m1, m2, m3) = pFoo y paG paD -- uncurryN?
                in pFoo m1 m2 m3
        go x' y' $ PollardAcc a' b' g' d'

    p1 = p `div` 3
    p2 = 2 * p1
    pFoo x a b
        | x < p1 = ( (g * x) `mod` p
                   , (a+1) `mod` (p-1)
                   , b)
        | x < p2 = ( (x * x) `mod` p
                   , (2*a) `mod` (p-1)
                   , (2*b) `mod` (p-1))
        | otherwise = ( (h*x) `mod` p
                      , a
                      , (b+1) `mod` (p-1))

{-
λ> pollardRho 7963 7 3018
5238
-}

----------------------------------------------------------------------------
-- 5.42
----------------------------------------------------------------------------

{-
λ> pollardRho 5011 2 2495
3351
λ> pollardRho 17959 17 14226
14557
λ> pollardRho 15239131 29 5953042
2528453

The last one took a while even with -O2 :\
-}
