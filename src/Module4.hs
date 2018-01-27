{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | RSA signatures, DSA.

module Module4 () where

import Universum hiding (exp)

import Data.List ((!!))
import Data.Numbers.Primes (primes)
import System.Random (randomRIO)

import Lib (exp, inverse)

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
  where
    suchThat :: (Monad m) => m a -> (a -> Bool) -> m a
    suchThat action predicate = do
        x <- action
        if predicate x then pure x else action `suchThat` predicate

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
