{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | RSA signatures, DSA.

module Module4 () where

import Universum hiding (exp)

import Data.List (nub, (!!))
import Data.Numbers.Primes (primeFactors, primes)
import System.Random (randomRIO)

import Lib (exp, inverse, suchThat)

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
    let rsP = 514
    let rsQ = 1223
    let rsE = 159853
    let rsD = inverse ((rsP - 1) * (rsQ - 1)) rsE
    let sk = RdsSk{..}
    print $ rsN sk
    print rsD
    print $ rdsSign sk 630579

{-
λ> e41
628622
75562
RdsSignature 227023
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

data ElgamalSetup = ElgamalSetup { eP :: Integer, eG :: Integer } deriving Show
data ElgamalPk = ElgamalPk { epS :: ElgamalSetup, epA :: Integer } deriving Show
data ElgamalSk = ElgamalSk { esS :: ElgamalSetup, esA :: Integer } deriving Show
data ElgamalSig = ElgamalSig { s1 :: Integer, s2 :: Integer } deriving Show

elgamalToPublic :: ElgamalSk -> ElgamalPk
elgamalToPublic ElgamalSk{..} = ElgamalPk esS (exp (eP esS) (eG esS) esA)

elgamalSignRaw :: ElgamalSk -> Integer -> Integer -> ElgamalSig
elgamalSignRaw ElgamalSk{..} d k = ElgamalSig{..}
  where
    s1 = exp eP eG k `mod` eP
    s2 = ((d - esA * s1) * (inverse k (eP-1))) `mod` eP
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
ElgamalSig {s1 = 3534, s2 = 2821}
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

{-

data ElgamalPk = ElgamalPk { eP :: Integer, eQ :: Integer, eG :: Integer }
data ElgamalSk = ElgamalSk { ePk :: ElgamalPk, eA :: Integer }
data ElgamalSig = ElgamalSig { s1 :: Integer, s2 :: Integer }

elgamalSignRaw :: ElgamalSk -> Integer -> Integer -> ElgamalSig
elgamalSignRaw ElgamalSk{..} d k = ElgamalSig{..}
  where
    s1 = exp eP eG k `mod` eQ
    s2 = ((d + eA * s1) * (inverseP k eQ)) `mod` eQ
    ElgamalPk{..} = ePk

elgamalSign :: ElgamalSk -> Integer -> IO ElgamalSig
elgamalSign sk d = elgamalSignRaw sk d <$> randomRIO (10, eQ (ePk sk) - 1)


-}
