{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | RSA signatures, DSA.

module Module4 () where

import Universum hiding (exp)

import Data.List (nub, (!!))
import Data.Numbers.Primes (primeFactors, primes)
import System.Random (randomRIO)

import Lib (exp, inverse, inverseP, logDShank, suchThat)

----------------------------------------------------------------------------
-- RSA Digital Signatures
----------------------------------------------------------------------------

rsN :: RdsSk -> Integer
rsN RdsSk{..} = rsP * rsQ

data RdsSk = RdsSk
    { rsP :: Integer
    , rsQ :: Integer
    , rsE :: Integer
    , rsD :: Integer
    } deriving (Show,Eq,Ord)

data RdsPk = RdsPk
    { rpN :: Integer
    , rpE :: Integer
    } deriving (Show,Eq,Ord)

newtype RdsSignature = RdsSignature Integer deriving Show

rdsToPublic :: RdsSk -> RdsPk
rdsToPublic rs = RdsPk (rsN rs) (rsE rs)

-- | Highly unsafe, no conditions on e/d are checked.
rdsKeyGen :: IO (RdsSk, RdsPk)
rdsKeyGen = do
    rsP <- (primes !!) <$> randomRIO (10001,20000)
    rsQ <- (primes !!) <$> randomRIO (20001,30000)
    let n = rsP * rsQ
    let s = (rsP-1)*(rsQ-1)
    rsE <- randomRIO (1,n-1) `suchThat` (\e -> gcd e s == 1)
    let rsD = inverse rsE s
    let sk = RdsSk{..}
    let pk = rdsToPublic sk
    pure (sk,pk)

rdsSign :: RdsSk -> Integer -> RdsSignature
rdsSign rs@RdsSk{..} v = RdsSignature $ exp n (v `mod` n) rsD
  where
    n = rsN rs

rdsValidate :: RdsPk -> Integer -> RdsSignature -> Bool
rdsValidate RdsPk{..} d (RdsSignature sig) = exp rpN sig rpE == d

----------------------------------------------------------------------------
-- 4.1
----------------------------------------------------------------------------

e41 :: IO ()
e41 = do
    let rsP = 541
    let rsQ = 1223
    let rsE = 159853
    let rsD = inverse ((rsP - 1) * (rsQ - 1)) rsE
    let sk = RdsSk{..}
    print $ rsN sk
    print rsD
    print $ rdsSign sk 630579

{-
λ> e41
661643
23828
RdsSignature 123518
-}

----------------------------------------------------------------------------
-- 4.2
----------------------------------------------------------------------------

e42 :: IO ()
e42 =
    mapM_
        (print .
         uncurry (rdsValidate (RdsPk 1562501 87953)) . second RdsSignature)
        [(119812,876453),(161153,870099),(586036,602754)]

{-
λ> e42
False
True
True
-}

----------------------------------------------------------------------------
-- 4.3
----------------------------------------------------------------------------

breakRsa :: RdsPk -> RdsSk
breakRsa RdsPk {..} =
    let [rsP, rsQ] = nub $ primeFactors rpN
    in RdsSk rsP rsQ rpE (inverse rpE ((rsP-1)*(rsQ-1)))

e43 :: IO ()
e43 = do
    let sk = breakRsa (RdsPk 27212325191 22824469379)
    let d = 12910258780
    print $ rdsSign sk d

{-
λ> e43
RdsSignature 22054770669
-}

----------------------------------------------------------------------------
-- 4.4
----------------------------------------------------------------------------

{-
Well, it _obviously_ works, because it's just a composition of two primitives.

Regarding security, it's a hard task to show that it is hard to break,
though we can reason in this way:

* It's impossible to get Hash(m) without knowing m, safety of which is
  guaranteed by RSA PSK. RSA DS doesn't disclose m as well, because
  Hash(m)^e has two levels of security: it's hard to get Hash(m)
  (would require knowing d, which would break RSA PKC), it's also hard
  to reverse hash.

* It's thus impossible to forge Hash(m).

-}

----------------------------------------------------------------------------
-- Elgamal signature scheme
----------------------------------------------------------------------------

data ElgamalSetup = ElgamalSetup { eP :: Integer, eG :: Integer } deriving (Show,Eq)
data ElgamalPk = ElgamalPk { epS :: ElgamalSetup, epA :: Integer } deriving (Show,Eq)
data ElgamalSk = ElgamalSk { esS :: ElgamalSetup, esA :: Integer } deriving (Show,Eq)
data ElgamalSig = ElgamalSig { s1 :: Integer, s2 :: Integer } deriving (Show,Eq)

elgamalToPublic :: ElgamalSk -> ElgamalPk
elgamalToPublic ElgamalSk{..} = ElgamalPk esS (exp (eP esS) (eG esS) esA)

elgamalSignRaw :: ElgamalSk -> Integer -> Integer -> ElgamalSig
elgamalSignRaw ElgamalSk{..} d k = ElgamalSig{..}
  where
    s1 = exp eP eG k `mod` eP
    s2 = ((d - esA * s1) * (inverse k (eP-1))) `mod` (eP - 1)
    ElgamalSetup{..} = esS

elgamalSign :: ElgamalSk -> Integer -> IO ElgamalSig
elgamalSign sk d =
    fmap (elgamalSignRaw sk d) $
    randomRIO (10, eP (esS sk) - 2) `suchThat` (\k -> gcd k (eP $ esS sk) == 1)

elgamalVerify :: ElgamalPk -> Integer -> ElgamalSig -> Bool
elgamalVerify pk d ElgamalSig{..} =
    ((exp eP (epA pk) s1 * exp eP s1 s2) `mod` eP) == exp eP eG d
  where
    ElgamalSetup{..} = epS pk

----------------------------------------------------------------------------
-- 4.5
----------------------------------------------------------------------------

e45 :: IO ()
e45 = do
    let setup = ElgamalSetup 6961 437
    let sk = ElgamalSk setup 6104
    let pk = elgamalToPublic sk
    print pk
    print $ elgamalSignRaw sk 5584 4451
{-
λ> e45
ElgamalPk {epS = ElgamalSetup {eP = 6961, eG = 437}, epA = 2065}
ElgamalSig {s1 = 3534, s2 = 5888}
-}

----------------------------------------------------------------------------
-- 4.6
----------------------------------------------------------------------------

e46 :: IO ()
e46 = do
    let setup = ElgamalSetup 6961 437
    let pk = ElgamalPk setup 4250
    mapM_ (print . uncurry (elgamalVerify pk))
        [ (1521, ElgamalSig 4129 5575)
        , (1837, ElgamalSig 3145 1871)
        , (1614, ElgamalSig 2709 2994)
        ]

{-
λ> e46
True
False
True
-}

----------------------------------------------------------------------------
-- 4.7
----------------------------------------------------------------------------

{-
A^{S1} * S1^S2 =
  = A^{S1} * A^(S2*j) * g^(S2*i)
  = A^{S1 + j*(- j^(-1)*S1)} * g^(S2*i)
  = g^(S2*i)
  = g^(-S1*i/j) = g^D
-}

----------------------------------------------------------------------------
-- 4.8
----------------------------------------------------------------------------

recoverElgamal :: ElgamalPk -> (Integer, ElgamalSig) -> (Integer,ElgamalSig) -> Maybe ElgamalSk
recoverElgamal pk (d1,(ElgamalSig s1 s2)) (d2,(ElgamalSig _ s2')) = do
    guard $ exp eP eG k == s1
    guard $ elgamalToPublic sk == pk
    pure sk
  where
    sk = ElgamalSk (epS pk) a
    ElgamalSetup{..} = epS pk
    -- solves `x*s = y mod r` for s
    solve x y =
        if gcd x r == 1
        then (y * inverse x r) `mod` r
        else fromMaybe (error "recoverElg: must exist") $
             find (\s -> (s * x) `mod` r == y) [1..r `div` (gcd x r)]
    a = solve s1 $ (d1 - (k * s2) `mod` r) `mod` r
    k = solve sdiff ddiff
    ddiff = (d1 - d2) `mod` r
    sdiff = (s2 - s2') `mod` r
    r = eP - 1

e48 :: Maybe ElgamalSk
e48 =
    recoverElgamal
        (ElgamalPk (ElgamalSetup 348149 113459) 185149)
        (153405, ElgamalSig 208913 209176)
        (127561, ElgamalSig 208913 217800)

{-
(a) S1 will be the same, because S1 = g^k.
(b) We need to solve the following system:

k*s2 + a*s1 = d
k*s2' + a*s1' = d'

All operations are done modulo p-1. It's trivial to do, except for
the taking inverse moment: sometimes solving ax = b (mod p-1)
can't be solved by taking a^-1, so we need to do trial and error.

(c) Just (ElgamalSk {esS = ElgamalSetup {eP = 348149, eG = 113459}, esA = 72729})
-}


----------------------------------------------------------------------------
-- DSA
----------------------------------------------------------------------------

data DsaSetup = DsaSetup { dP :: Integer, dQ :: Integer, dG :: Integer } deriving (Show,Eq)
data DsaPk = DsaPk { dpS :: DsaSetup, dpA :: Integer } deriving (Show,Eq)
data DsaSk = DsaSk { dsS :: DsaSetup, dsA :: Integer } deriving (Show,Eq)
data DsaSig = DsaSig { ds1 :: Integer, ds2 :: Integer } deriving (Show,Eq)

dsaToPublic :: DsaSk -> DsaPk
dsaToPublic DsaSk{..} = DsaPk dsS (exp (dP dsS) (dG dsS) dsA)

dsaSignRaw :: DsaSk -> Integer -> Integer -> DsaSig
dsaSignRaw DsaSk{..} d k = DsaSig{..}
  where
    ds1 = exp dP dG k `mod` dQ
    ds2 = ((d + dsA * ds1) * (inverseP k dQ)) `mod` dQ
    DsaSetup{..} = dsS

dsaSign :: DsaSk -> Integer -> IO DsaSig
dsaSign sk d = dsaSignRaw sk d <$> randomRIO (10, dQ (dsS sk) - 1)

dsaVerify :: DsaPk -> Integer -> DsaSig -> Bool
dsaVerify DsaPk{..} d DsaSig{..} =
    ((exp dP dG v1 * exp dP dpA v2) `mod` dP) `mod` dQ == ds1
  where
    DsaSetup{..} = dpS
    v1 = (d * inverseP ds2 dQ) `mod` dQ
    v2 = (ds1 * inverseP ds2 dQ) `mod` dQ

----------------------------------------------------------------------------
-- 4.9
----------------------------------------------------------------------------

e49 :: IO ()
e49 = do
    let setup = DsaSetup 22531 751 4488
    let sk = DsaSk setup 674
    let pk = dsaToPublic sk
    print pk
    print $ dsaSignRaw sk 244 574

    x <- dsaSign sk 123
    print x
    print $ dsaVerify pk 123 x


{-
λ> e49
DsaPk {dpS = DsaSetup {dP = 22531, dQ = 751, dG = 4488}, dpA = 4940}
DsaSig {ds1 = 444, ds2 = 56}
-}

----------------------------------------------------------------------------
-- 4.10
----------------------------------------------------------------------------

e410 :: IO ()
e410 = do
    let setup = DsaSetup 22531 751 4488
    let pk = DsaPk setup 22476
    mapM_ (print . uncurry (dsaVerify pk)) $ [(329, DsaSig 183 260), (432, DsaSig 211 97)]

{-
λ> e410
True
False
-}

----------------------------------------------------------------------------
-- 4.11
----------------------------------------------------------------------------

collisionDsa :: DsaPk -> DsaSk
collisionDsa DsaPk{..} = let DsaSetup{..} = dpS in DsaSk dpS (logDShank dP dG dpA)

e411 :: IO ()
e411 = do
    let setup = DsaSetup 103687 1571 21947
    let pk = DsaPk setup 31377
    let sk = collisionDsa pk
    print $ dsaSignRaw sk 510 1105

{-
λ> e411
DsaSig {ds1 = 439, ds2 = 1259}
-}
