{-# LANGUAGE NoImplicitPrelude #-}

module Module1v3 () where

import           Prelude hiding ((++))

m = 15

units :: Int -> [Int]
units m = filter (\n -> gcd n m == 1) [1..m-1]

(*:), (+:) :: Int -> Int -> Int
(*:) a b = (a * b) `mod` m
(+:) a b = (a + b) `mod` m

--- ?
