{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Module1v3 () where

import Prelude hiding ((++))

m :: Int
m = 15

units :: Int -> [Int]
units k = filter (\n -> gcd n m == 1) [1..k-1]

(*:), (+:) :: Int -> Int -> Int
(*:) a b = (a * b) `mod` m
(+:) a b = (a + b) `mod` m
