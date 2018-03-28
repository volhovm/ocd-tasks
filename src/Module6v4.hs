{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | ECC primitives.

module Module6v4 () where

import Universum hiding ((<*>))

import Lib.Elliptic
import Lib.Field

import Module2v8 (sqrtPN)
import Module6v3 (ecRho)

sqrts :: forall n. (KnownNat n) => Z n -> [Z n]
sqrts (Z i) = map toZ $ sqrtPN (toInteger $ natVal (Proxy @n)) i

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
    let (ya:_) = sqrts yasq
    let qa2 = EC xa ya
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
    let xa = toZ 278 :: Z 1123
    let ECParams{..} = ecParams
    let yasq = xa <^> (3 :: Int) <+> ecA <*> xa <+> ecB
    let sols = sqrtPN 1123 (unZ yasq)
    print sols

{-
(a) We compute s = x^3 + ax + b and then take a square root of s. The key point
here is that s ∈ F_p, so that if t^2 = s, (-t)^2 = s. As we know, when taking
a minus, we take a number to the other half of [0..p]. That's why it's sufficient
to send 1 bit, following the given rules.

(b)
λ> e616
[487,636]

He'll send 0 to denote 487 and 1 to denote 636. Sanity check: the sum
of these numbers is 1123.
-}

----------------------------------------------------------------------------
-- 6.17 MV-Elgamal
----------------------------------------------------------------------------

e617c :: IO ()
e617c = withECParams (ECParams (toZ 19) (toZ 17) :: ECParams (Z 1201)) $ do
    let (p :: EC (Z 1201)) = EC 278 285
    let na = 595 :: Integer
    let qa = na `times` p
    putText $ "Qa = " <> show qa
    let (r :: EC (Z 1201)) = EC 1147 640
    let c1 = toZ 279
    let c2 = toZ 1189
    let EC xt yt = na `times` r
    let m1 = (finv xt) <*> c1
    let m2 = (finv yt) <*> c2
    putText $ "m1,m2 = " <> show (m1,m2)

{-
(a) S is essentially T. Both Bob and Alice end up with S = T = k * na * P,
which is something like a one-time symmetric key to decrypt c1 and c2.

(b) It encodes two ciphertexts m1, m2 ∈ F_p, so that's 2 * log(p) bit
of original data. We also send R, which is a pair of numbers of log(p)
size as well. Alternatively, we can send only one log(p) number
(x coordinate) and boolean value for y, as suggested in 6.16, then
there'll be only 1 + log(p) extra data. Message ratio is (3x+1)/2x ~ 3/2.

(c)
λ> e617c
Qa = EC 1104 492
m1,m2 = (509,767)
-}

----------------------------------------------------------------------------
-- 6.18 MV-Elgamal 2
----------------------------------------------------------------------------

{-
(a) c1 = xs * m1
    c2 = ys * m2
    m1 = c1 * (xs^(-1))
    m2 = c2 * (ys^(-1))
    xs = c1 / m1
    ys = c2 / m2

    But (c2/m2)^2 = (c1/m1)^3 + a*(c1/m1) + b

    Let's suppose Eve knows m1. Then,
    c2^2 * (m2^(-1))^2 = (c1/m1)^3 + a(c1/m1) + b
    (m2^(-1))^2 = ((c1/m1)^3 + a(c1/m1) + b) / c2^2
    m2^(-1) = sqrt[ ((c1/m1)^3 + a(c1/m1) + b) / c2^2 ]
    m2 = 1 / sqrt[ ((c1/m1)^3 + a(c1/m1) + b) / c2^2 ]
-}

e618 :: IO ()
e618 = withECParams (ECParams (toZ 19) (toZ 17) :: ECParams (Z 1201)) $ do
    let (c1 :: Z 1201) = toZ 814
    let c2 = toZ 1050
    let m1 = toZ 1050
    let pow2 x = x <^> (2 :: Integer)
    let pow3 x = x <^> (3 :: Integer)
    let ECParams{..} = ecParams
    let res = map finv $ sqrts $
              (pow3 (c1 <*> finv m1) <+> ecA <*> (c1 <*> finv m1) <+> ecB) <*>
              finv (pow2 c2)
    print res

{-
(b)
λ> e618
[179,1022]
-}

----------------------------------------------------------------------------
-- 6.19 EC Elgamal DS
----------------------------------------------------------------------------

{-

P is known. Alice chooses a as her secret key and computes Q = aP.

She also chooses random k and computes R = kP.

Then she sends:
t = (D - a)k^(-1)
R = kP

And then Q + tR = (aP + (D-a)k^(-1)kP) = (aP + (D-a)P) = DP. We can
compare that to DP.

TODO I may be missing something here. What happens to the original
Elgamal DS if we take S2 = (D - a)k^(-1) and then compute A*S1^(S2)?
It'll be equal to g^D anyway, but are we losing security?

-}

----------------------------------------------------------------------------
-- 6.20 ECDSA
----------------------------------------------------------------------------

ecdsaSign ::
       forall p q. (PrimeNat p, PrimeNat q, HasECParams (Z p))
    => EC (Z p)
    -> Z q
    -> Z q
    -> Z q
    -> (Z q, Z q)
ecdsaSign g s d e =
    let eg = e `times` g
        s1 :: Z q = let EC x _ = eg in toZ (unZ x)
        s2 :: Z q = (d <+> s <*> s1) <*> finv e
    in (s1, s2)

ecdsaVerify ::
       forall p q. (PrimeNat p, PrimeNat q, HasECParams (Z p))
    => EC (Z p)
    -> EC (Z p)
    -> (Z q, Z q)
    -> Z q
    -> Bool
ecdsaVerify g v (s1,s2) d =
    let v1 = d <*> finv s2
        v2 = s1 <*> finv s2
        t :: EC (Z p) = v1 `times` g <+> v2 `times` v
        xfin :: Z q = let EC x _ = t in toZ (unZ x)
    in xfin == s1

e620 :: IO ()
e620 = withECParams (ECParams (toZ 231) (toZ 473) :: ECParams (Z 17389)) $ do
    let (g :: EC (Z 17389)) = EC 11259 11278
    let q = 1321
    unless (ecOrder g == q) $ error "order of g is not q"

    let s = toZ 542 :: Z 1321
    let qa = s `times` g
    print qa

    print $ ecdsaSign g s 644 847

    print $ ecdsaVerify @17389 @1321 g (EC 11017 14637) (toZ 907, toZ 296) 993

    let up = EC 14594 308 :: EC (Z 17389)
    let us = toZ $ ecRho g up
    print $ ecdsaSign @17389 @1321 g us 516 365

{-
λ> e620
EC 8689 1726
(491,290)
True
(1281,236)
-}
