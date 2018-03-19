{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | ECC primitives.

module Module6v4 () where

import Universum hiding ((<*>))

import Lib.Elliptic
import Lib.Field

import Module2v8 (sqrtPN)
import Module6v3 (ecRho)

----------------------------------------------------------------------------
-- 6.14 EC D-H
----------------------------------------------------------------------------

e614 :: IO ()
e614 = withECParams (ECParams (toZ 171) (toZ 853) :: ECParams (Z 2671)) $ do
    let (p :: EC (Z 2671)) = EC 1980 431
    let (qa :: EC (Z 2671)) = EC (toZ 2110) (toZ 543)
    let nb = 1943 :: Integer
    let qb = nb `times` p
    putText $ "(a) " <> show qb
    let secval = nb `times` qa
    putText $ "(b) " <> show secval
    let na = ecRho p qa
    putText $ "(c) " <> show na
    -- d
    let xa = toZ 2 :: Z 2671
    let ECParams{..} = ecParams
    let yasq = xa <^> (3 :: Int) <+> ecA <*> xa <+> ecB
    let Just (ya:_) = sqrtPN 2671 (unZ yasq)
    let qa2 = EC xa (toZ ya)
    putText $ "(d) Q_a: " <> show qa2
    let nb2 = 875 :: Integer
    let qb2 = nb2 `times` p
    putText $ "    Q_b: " <> show qb2  -- x coordinate
    putText $ "    secval: " <> show (nb2 `times` qa2) -- secret value is x

{-
λ> e614
(a) EC 1432 667
(b) EC 2424 911
(c) 726
(d) Q_a: EC 2 2575
    Q_b: EC 161 2040
    secval: EC 1708 1419
-}
