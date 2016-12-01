module Module2v9 where

import           Control.Exception.Base (assert)
import           Control.Monad          (forM_, unless)
import           Data.List              (group)
import           Data.Maybe             (fromMaybe, isJust, isNothing)
import           Data.Numbers.Primes    (isPrime, primeFactors)
import           Debug.Trace
import           Prelude                hiding (exp)

import           Lib                    (crt, exp, logD, logDShank, logDTrialAndError,
                                         order)


----------------------------------------------------------------------------
-- 2.26
----------------------------------------------------------------------------

{-
Well, obviously, if g^(p-1) ≡ 1 (mod p), then g^(n*N) ≡ (g^n)^N ≡ 1 (mod p).
If N is prime itself, then g^N is the needed element.
Otherwise repeat procedure.
-}

----------------------------------------------------------------------------
-- 2.27
----------------------------------------------------------------------------

-- I did it on the paper. Not a big deal.

----------------------------------------------------------------------------
-- 2.28
----------------------------------------------------------------------------

logDPohligHellman :: Integer -> Integer -> Integer -> Integer
logDPohligHellman p g h
    | h == 1 = logD p g h
    | isNothing _N0 = logD p g h
    | length factors == 1 = logD p g h
    | otherwise = assert (exp p g solution == h) solution
  where
    solution = crt subLogs
    subLogs = flip map factors $ \(qi,ei) ->
        let qiei = exp p qi ei
            ni = _N `div` qiei
            gi = exp p g ni
            hi = exp p h ni
            yi = logDShank p gi hi
        in
--           trace ("qiei = " ++ show qiei) $
--           trace ("ni = " ++ show ni) $
--           trace ("gi = " ++ show gi) $
--           trace ("hi = " ++ show hi) $
--           trace ("yi = " ++ show yi) $
           (yi `mod` qiei,qiei)
    factors = case _N of
      1 -> [(1,1)]
      n -> groupMany $ primeFactors _N
    groupMany = map (\x -> (head x, fromIntegral $ length x)) . group
    _N = fromMaybe (error "logDPohligHellman called with bad g") _N0
    _N0 = order p g

testDPohlig :: IO ()
testDPohlig =
    forM_
        [ (p, g, b)
        | p <- [1 .. 100]
        , g <- [2 .. 100]
        , b <- [1 .. 100]
        , g < p
        , b < p] $
    \(p, g, b) -> do
        let h = exp p g b
        let label = show g ++ "^" ++ show b ++ " ~? " ++ show h ++ " (mod " ++ show p ++ ")"
--        putStrLn label
        let b = logDPohligHellman p g h
        unless (exp p g b == h) $ error $ "pohlig-hellman failed: " ++ label

{-
λ> logDPohligHellman 433 7 166
47
λ> logDPohligHellman 746497 10 243278
223755
λ> logDPohligHellman 41022299 2 39183497
33703314
λ> logDPohligHellman 1291799 17 192988
984414
-}
