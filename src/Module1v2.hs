{-# LANGUAGE NoImplicitPrelude #-}
module Module1v2 () where

import           Control.Monad  (forM_, unless)
import           Data.Bifunctor (bimap, second)
import           Data.Bool      (bool)
import           Data.Tuple     (swap)
import           Prelude        hiding (gcd)

gcd :: Int -> Int -> Int
gcd a b = bool (gcd b r) b (q * b == a)
  where
    r = a `mod` b
    q = a `div` b

gcd' :: Int -> Int -> (Int,Int)
gcd' a b | a < b = swap $ gcd' b a
gcd' a b = last $ takeWhile ((/= 0) . rieval') $ map ri [0..]
  where
    bimap' f = bimap f f
    bisum (x,y) (c,d) = (x+c,y+d)
    qi :: Int -> Int
    qi 1 = a `div` b
    qi 2 =  b `div` rieval 1
    qi i = rieval (i-2) `div` rieval (i-1)
    rieval i = rieval' $ ri i
    rieval' (ak,bk) = ak*a+bk*b
    ri :: Int -> (Int, Int) -- a's, b's, value
    ri 0 = (0, 1)
    ri 1 = (1, -(a `div` b))
    ri 2 = second (+1) $ bimap' (negate . (* qi 2)) $ ri 1
    ri i = bisum (ri $ i-2) $ bimap' (negate . (* qi i)) $ ri (i-1)

testGcd :: IO ()
testGcd =
    forM_ [(a,b) | a <- [1..100], b <- [1..100]] $
      \(a,b) -> do
        --putStrLn (show a ++ "~" ++ show b)
        unless (gcd a b == (let (ka,kb) = gcd' a b in ka * a + kb * b)) $
            error (show a ++ "~" ++ show b)

main :: IO ()
main = undefined

{-
1.11
a) http://math.stackexchange.com/questions/219941/is-greatest-common-divisor-of-two-numbers-really-their-smallest-linear-combinati
thus there's no lowest positive number then 1, 1 is gcd

b) gcd (16,10) = 2, but 16*1-10*1 = 6.
c)
-}
