{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Elliptic curves over finite fields.

module Module6v2 () where

import Universum

import Lib.Elliptic
import Lib.Field

----------------------------------------------------------------------------
-- 6.5
----------------------------------------------------------------------------

e65 :: IO ()
e65 = do
    let solve :: forall n. PrimeNat n => (Integer,Integer) -> IO ()
        solve (a,b) =
            withECParams (ECParams (toZ a) (toZ b) :: ECParams (Z n)) $
            print $ listAllPoints @(Z n)
    solve @7 (3,2)
    solve @11 (2,7)
    solve @11 (4,5)
    solve @11 (9,5)
    solve @13 (9,5)

{-
λ> e65
[EC 0 3,EC 0 4,EC 2 3,EC 2 4,EC 4 1,EC 4 6,EC 5 3,EC 5 4]
[EC 6 2,EC 6 9,EC 7 1,EC 7 10,EC 10 2,EC 10 9]
[EC 0 4,EC 0 7,EC 3 0,EC 6 5,EC 6 6,EC 9 0,EC 10 0]
[EC 0 4,EC 0 7,EC 1 2,EC 1 9,EC 2 3,EC 2 8,EC 3 2,EC 3 9,EC 6 0,EC 7 2,EC 7 9,EC 9 1,EC 9 10]
[EC 4 1,EC 4 12,EC 8 2,EC 8 11,EC 9 3,EC 9 10,EC 10 4,EC 10 9]
-}

----------------------------------------------------------------------------
-- 6.6
----------------------------------------------------------------------------

additionTable :: forall f. (Ord f, HasECParams f, FField f) => Matrix (EC f)
additionTable = Matrix $ do
    x <- listAllPoints @f
    let l = do y <- listAllPoints @f
               pure $ x <+> y
    pure l

e66 :: IO ()
e66 = do
    let solve :: forall n. PrimeNat n => (Integer,Integer) -> IO ()
        solve (a,b) =
            withECParams (ECParams (toZ a) (toZ b) :: ECParams (Z n)) $
            putStrLn $ showMatrix $ additionTable @(Z n)
    solve @5 (1,2)
    solve @7 (2,3)
    solve @11 (2,5)

{-
Does it help? :)

EC 4 0 EC0    EC 1 3
EC0    EC 4 0 EC 1 2
EC 1 3 EC 1 2 EC0

EC 3 6 EC0    EC 2 6 EC 6 0 EC 3 1
EC0    EC 3 1 EC 6 0 EC 2 1 EC 3 6
EC 2 6 EC 6 0 EC 3 6 EC0    EC 2 1
EC 6 0 EC 2 1 EC0    EC 3 1 EC 2 6
EC 3 1 EC 3 6 EC 2 1 EC 2 6 EC0

EC 9 2 EC0    EC 8 7 EC 9 9 EC 8 4 EC 3 7 EC 4 0 EC 3 4 EC 0 7
EC0    EC 9 9 EC 9 2 EC 8 4 EC 8 7 EC 4 0 EC 3 4 EC 0 4 EC 3 7
EC 8 7 EC 9 2 EC 8 4 EC0    EC 9 9 EC 0 7 EC 3 7 EC 4 0 EC 0 4
EC 9 9 EC 8 4 EC0    EC 8 7 EC 9 2 EC 3 4 EC 0 4 EC 0 7 EC 4 0
EC 8 4 EC 8 7 EC 9 9 EC 9 2 EC0    EC 0 4 EC 0 7 EC 3 7 EC 3 4
EC 3 7 EC 4 0 EC 0 7 EC 3 4 EC 0 4 EC 9 2 EC0    EC 9 9 EC 8 7
EC 4 0 EC 3 4 EC 3 7 EC 0 4 EC 0 7 EC0    EC 9 9 EC 8 4 EC 9 2
EC 3 4 EC 0 4 EC 4 0 EC 0 7 EC 3 7 EC 9 9 EC 8 4 EC 8 7 EC0
EC 0 7 EC 3 7 EC 0 4 EC 4 0 EC 3 4 EC 8 7 EC 9 2 EC0    EC 8 4

-}

----------------------------------------------------------------------------
-- 6.7
----------------------------------------------------------------------------

e67 :: IO ()
e67 = do
    let solve :: forall n. PrimeNat n => (Integer,Integer) -> IO ()
        solve (a,b) =
            withECParams (ECParams (toZ a) (toZ b) :: ECParams (Z n)) $ do
                let p :: Integer
                    p = natVal (Proxy @n)
                let l = length $ listAllPoints @(Z n)
                let frob = p - 1 - (fromIntegral l)
                putStrLn $ show l ++ " => " ++ show frob ++
                           " compared to " ++ show (2 * sqrt (fromInteger p) :: Double)
    solve @3 (1,1)
    solve @5 (1,1)
    solve @7 (1,1)
    solve @11 (1,1)
    solve @17 (1,1)

{-
λ> e67
4  => -2 compared to 3.46
9  => -5 compared to 4.47
5  =>  1 compared to 5.29
14 => -4 compared to 6.63
18 => -2 compared to 8.24
-}
