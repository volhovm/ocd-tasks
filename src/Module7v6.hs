{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Babai's CVP algorithm.

module Module7v6 () where

import Universum

import Lib.Lattice
import Lib.Vector

----------------------------------------------------------------------------
-- 7.17 Babai's algorithm
----------------------------------------------------------------------------

e717 :: IO ()
e717 = do
    let v1 = Vect [213,-437] :: Vect Integer
    let v2 = Vect [312,105] :: Vect Integer
    let w = Vect [43127,11349] :: Vect Integer

    let v = fst $ babaiCVP [v1,v2] (map fromIntegral w)
    putText $ "(a): " <> show v <> ", diff length " <> show (vlen $ v `vminus` w)

    let hm = hadamardRatio [v1,v2]
    putText $ "(b): " <> show hm

    let v1' = Vect [2937,-1555] :: Vect Integer
    let v2' = Vect [11223,-5888] :: Vect Integer
    let t = expressBaseInt [v1,v2] [v1',v2']
    putText $ "(c) Should be Just: " <> show t

    let v' = fst $ babaiCVP [v1',v2'] (map fromIntegral w)
    putText $ "(d): " <> show v' <> ", diff length " <> show (vlen $ v' `vminus` w)

    let hm' = hadamardRatio [v1',v2']
    putText $ "(e): " <> show hm'

{-
λ> e717
(a): Vect {unVect = [43086,11448]}, diff length 107.15409464878138
(b): 0.9958435668192238
(c) Should be Just: Just [Vect {unVect = [5,6]},Vect {unVect = [19,23]}]
(d): Vect {unVect = [46548,9561]}, diff length 3860.075776458281
(e): 6.1385591270477755e-2

(b) is almost orthogonal, (e) is not at all.
-}
