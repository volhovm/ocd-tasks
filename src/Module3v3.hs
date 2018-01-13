{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE ViewPatterns #-}

module Module3v3 () where

import Universum hiding (exp)

import Lib (eulerPhiFast, exEucl, exp)


----------------------------------------------------------------------------
-- 3.12
----------------------------------------------------------------------------

{-
As for any other public key cryptosystem w/o authentication, if pk
exchange can be intercepted, then the whole security framework is
flawed. The same scheme applies to both RSA and elgamal PKC:

1. Alice publishes her pk A
2. Eve intercepts this A and sends her E to Bob
3. Bob encrypts message m to E thinking that he's encrypting to A
4. Eve gets c' encrypted to her, decrypts, changes any info she wants to,
   encrypts to A and sends it to alice.
5. Alice thinks she got message from Bob, but it was changed by Eve.
-}

----------------------------------------------------------------------------
-- 3.13
----------------------------------------------------------------------------

rsaMultipleExponents ::
       Integer -> (Integer, Integer) -> (Integer, Integer) -> Maybe Integer
rsaMultipleExponents n (e1,c1) (e2,c2) = do
    guard $ gcd' == 1
    guard $ u * e1 + v * e2 == 1
    guard $ ((toField u) * e1 + (toField v) * e2) `mod` n == 1
    guard $ exp n ans e2 == c2
    guard $ exp n ans e1 == c1
    pure ans
  where
    p = toInteger $ eulerPhiFast n
    toField = (`mod` n)
    (gcd',u,v) = exEucl e1 e2
    ans = (exp n c1 (u `mod` p) * exp n c2 (v `mod` p)) `mod` n

{-
λ> rsaMultipleExponents 1889570071 (1021763679,1244183534) (519424709,732959706)
Just 1054592380
-}
