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

----------------------------------------------------------------------------
-- 6.15 Three-round cryptosystem
----------------------------------------------------------------------------

{-
Let P be the secret point in E(F_p) with order p, p is public
knowledge. Alice chooses a, Bob chooses b. a and b should be coprimes
with p.

Alice sends u = aP to Bob.

Bob sends v = bu to Alice.

Alice now takes s such that sa = 1 mod p and sends w = sv to Bob.

Bob takes t = b^(-1) mod p and decrypts P = tsbaP, because tsba = 1
mod p.
-}

----------------------------------------------------------------------------
-- 6.16 Sending x-coordinate only
----------------------------------------------------------------------------

e616 :: IO ()
e616 = withECParams (ECParams (toZ 54) (toZ 87) :: ECParams (Z 1123)) $ do
    let xa = toZ 287 :: Z 1123
    let ECParams{..} = ecParams
    let yasq = xa <^> (3 :: Int) <+> ecA <*> xa <+> ecB
    print $ sqrtPN 1123 (unZ yasq)

{-
(a) We compute s = x^3 + ax + b and then take a square root of s. The key point
here is that s ∈ F_p, so that if t^2 = s, (-t)^2 = s. As we know, when taking
a minus, we take a number to the other half of [0..p]. That's why it's sufficient
to send 1 bit, following the given rules.

(b)
λ> e616
Just [252,871]

He'll send 0 to denote 252 and 1 to denote 871. Sanity check: the sum
of these numbers is 1123.
-}
