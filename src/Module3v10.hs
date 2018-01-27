{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Probabilistic encryption, Goldwasser-Micali cryptosystem.

module Module3v10 () where

import Universum hiding (exp)
import Unsafe (unsafeHead)

import Data.List ((!!))
import Data.Numbers.Primes (primeFactors, primes)
import System.Random (randomIO, randomRIO)

import Lib (isSquareRoot)

----------------------------------------------------------------------------
-- Cryptosystem
----------------------------------------------------------------------------

-- | Public key.
data GMPk = GMPk
    { gmN :: Integer -- ^ N = pq
    , gmA :: Integer -- ^ a | (a/p) = (a/q) = -1
    } deriving Show

-- | Secret key.
data GMSk = GMSk
    { gmP  :: Integer -- ^ p dividing N
    } deriving Show

suchThat :: (Monad m) => m a -> (a -> Bool) -> m a
suchThat action predicate = do
    x <- action
    if predicate x then pure x else action `suchThat` predicate

genPair :: IO (GMSk, GMPk)
genPair = do
    pi' <- randomRIO (1000,2000)
    qi' <- randomRIO (2000,3000)
    let p = primes !! pi'
    let q = primes !! qi'
    let n = p * q
    let notsqr a b = (== -1) $ isSquareRoot a b
    a <- randomRIO (1,n) `suchThat` (\a -> notsqr a p && notsqr a q)
    pure (GMSk p, GMPk n a)

encryptGMRaw :: Integer -> Integer -> Bool -> Integer -> Integer
encryptGMRaw n a m r = (`mod` n) $ bool identity (* a) m $ (r * r `mod` n)

encryptGM :: GMPk -> Bool -> IO Integer
encryptGM GMPk{..} m = do
    r <- randomRIO (gmN `div` 10,gmN)
    pure $ encryptGMRaw gmN gmA m r

decryptGM :: GMSk -> Integer -> Bool
decryptGM GMSk {..} c =
    case isSquareRoot c gmP of
        1    -> False
        (-1) -> True
        _    -> error "decryptGM: malformed c"

verifyGM :: IO ()
verifyGM = do
    replicateM_ 1000 $ do
        (sk,pk) <- genPair
        b <- randomIO
        c <- encryptGM pk b
        unless (decryptGM sk c == b) $ error $ "GM is broken: " <> show (sk,pk,c)
    putText "GM test passed"

----------------------------------------------------------------------------
-- 3.42 Playing around
----------------------------------------------------------------------------

e342a :: IO ()
e342a = do
    let gmsk = GMSk 32411
    let cs = [1794677960,525734818,420526487]
    print $ map (decryptGM gmsk) cs

{-
λ> e342a
[True,False,True]
-}

crackPk :: GMPk -> GMSk
crackPk GMPk{..} = GMSk $ unsafeHead $ primeFactors gmN

e342b :: IO ()
e342b = do
    let bobPk = GMPk 3149 2013
    let cs = [2322,719,202]
    let sk = crackPk bobPk
    print $ map (decryptGM sk) cs

{-
λ> e342b
[True,False,False]
-}

e342c :: IO ()
e342c =
    print $
    map (uncurry $ encryptGMRaw 781044643 568980706)
        [(True,705130839), (True,631364468), (True,67651321)]

{-
λ> e342c
[517254876,4308279,737604917]
-}


----------------------------------------------------------------------------
-- 3.43 Simple probabilistic encryption (adding the seed)
----------------------------------------------------------------------------

{-
(a)
m = d_k(c), then we split it into two parts: m_1 and m_2.
Because m_2 = (m' ⊕ m_1), m' = m_2 ⊕ m_1.

(b) 2

(c) μ * 2
-}
