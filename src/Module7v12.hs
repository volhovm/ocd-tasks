{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Lattice-based signature schemes.

module Module7v12 () where

import Universum hiding ((<*>))

import qualified Data.List as L

import Lib.Lattice
import Lib.Vector

import Module7v8 (genU)

data GghSigSk = GghSigSk
    { gssGoodBasis :: [Vect Integer]
    , gssBadBasis  :: [Vect Integer]
    } deriving (Show, Eq)

data GghSigPk = GghSigPk
    { gspBadBasis  :: [Vect Integer]
    } deriving (Show, Eq)

toPublic :: GghSigSk -> GghSigPk
toPublic GghSigSk{..} = GghSigPk gssBadBasis

newtype GghDoc = GghDoc (Vect Integer) deriving (Show)
newtype GghSig = GghSig (Vect Integer) deriving (Show)

gghSign :: GghSigSk -> GghDoc -> GghSig
gghSign GghSigSk{..} (GghDoc doc) =
    let (sol,_ks) = babaiCVP gssGoodBasis (map fromIntegral doc)
    in GghSig $ fromMaybe (error "gghSign failed") $ expressInt gssBadBasis sol

gghVerify :: GghSigPk -> Integer -> GghDoc -> GghSig -> Bool
gghVerify GghSigPk{..} dist (GghDoc doc) (GghSig sig) =
    let e = applyBase gspBadBasis sig
    in vlen (e `vminus` doc) <= fromIntegral dist

----------------------------------------------------------------------------
-- 7.39 Basic GGH
----------------------------------------------------------------------------

e739 :: IO ()
e739 = do
    let good = map Vect [[-20,-8,1],[14,11,23],[-18,1,-12]]
    let bad = map Vect [[-248100,220074,332172],[-112192,99518,150209],[-216150,191737,289401]]
    let sk = GghSigSk good bad
    let doc = GghDoc $ Vect [834928,123894,77812738]
    let sig = gghSign sk doc
    print $ sig
    print $ gghVerify (toPublic sk) 20 doc sig

----------------------------------------------------------------------------
-- 7.40 Basic GGH
----------------------------------------------------------------------------

e740 :: IO ()
e740 = do
    let w1 = [3712318934,-14591032252,11433651072]
    let w2 = [-1586446650,6235427140,-4886131219]
    let w3 = [305711854,-1201580900,941568527]
    let base = map Vect [w1,w2,w3]
    let pk = GghSigPk base
    let doc = GghDoc $ Vect [5269775,7294466,1875937]
    let sig = GghSig $ Vect [6987814629,14496863295,-9625064603]
    print $ gghVerify pk 60 doc sig

----------------------------------------------------------------------------
-- 7.41 GGH + LLL
----------------------------------------------------------------------------

-- TODO: LLL is the next section, I dunno.

-- notice: this naive approach doesn't work *at all*
stupidReduction0 :: Double -> [Vect Integer] -> IO [Vect Integer]
stupidReduction0 ratio = go
  where
    go b | hadamardRatio b > ratio = pure b
         | otherwise = do
               let tryReduce = do
                       u <- genU (length b)
                       let m = mToVecs $ u `mmulm` mFromVecs b
                       if hadamardRatio m > hadamardRatio b
                           then pure m
                           else tryReduce
               l <- tryReduce
               putText $ "Reduced, new ratio: " <> show (hadamardRatio l)
               go l

-- Iterating is terribly slow, doesn't work too.
stupidReduction1 :: Double -> Integer -> [Vect Integer] -> Maybe [Vect Integer]
stupidReduction1 ratio high bad =
    let n = length bad
        k = length $ L.head bad
        range = [-high..high]
        vecs = replicateM k range
        bases = replicateM n vecs

        sameBase b' = isJust $ expressBaseInt bad (map Vect b')
    in map Vect <$>
       find (\base -> hadamardRatio (map Vect base) >= ratio && sameBase base) bases

e741 :: IO ()
e741 = do
    let w1 = [-1612927239,1853012542,1451467045]
    let w2 = [-2137446623,2455606985,1923480029]
    let w3 = [2762180674,-3173333120,-2485675809]
    let base = map Vect [w1,w2,w3]
    -- "good" base
    let base' = snd $ lllReduction base :: [Vect Integer] -- stupidReduction 0.1 20 base
    print base'
    print $ hadamardRatio base
    print $ hadamardRatio base'
    let sk = GghSigSk base' base
    print sk

    let doc = Vect [87398273893,763829184,118237397273]
    let (sol,_) = babaiCVP base' (map fromIntegral doc)
    let diff = vlen $ sol `vminus` doc
    print diff

{-
The distance is 62 < 100.

λ> e741
[Vect {unVect = [118,15,26]},Vect {unVect = [-9,-147,-136]},Vect {unVect = [-45,154,-67]}]
4.052244528409397e-8
0.9385291371768608
GghSigSk {gssGoodBasis = [Vect {unVect = [118,15,26]},Vect {unVect = [-9,-147,-136]},Vect {unVect = [-45,154,-67]}], gssBadBasis = [Vect {unVect = [-1612927239,1853012542,1451467045]},Vect {unVect = [-2137446623,2455606985,1923480029]},Vect {unVect = [2762180674,-3173333120,-2485675809]}]}
61.59545437773797
-}

----------------------------------------------------------------------------
-- 7.42 NTRUMLS
----------------------------------------------------------------------------

{-

(a) ci = |sum{..} ai*bj| ≤ sum{..} |ai*bj|
                         ≤ N * |ai*bj|
                         ≤ N * p^2/4 ≤ B

(b) All straightforward

    t = hs
    t0+ag = h(s0+af)
    h*s0+ag = h(s0+af)
    ag = haf
    g = hf

    s0 + af = sp + pr + af = sp + pr + apF = sp (mod p)

    t0+ag = t0 + tp - t0 = tp

(c) t = t0 + ag.

    Norms (meaning ∥∙∥_{∞}):
    |t0| < 1/2q
    |a| < 1/2p
    |g| < 1/2p

    Thus by (a), |ag| < B and t < 1/2q + B


    s = s0 + af = sp  + pr + af
    |s|         ≤ p/2 + pA + p^2/2
                ≤ p/2 + q/2 - p/2 + p^2/2
                = q/2 + p^2/2
                ≤ q/2 + Np^2/4             (N ≥ 2)
                = B

(d) This is somewhat confusing. The probability of success is
    obviously P = [(q-2B)/(q+2B)]^(2N).

    And if we assume that B → 0 (???), then it's exactly

    (1 - 4B/q)^(2N) = e^(-8NB/q) = e^(-8/k)

    But I have no understanding why would we assume that.

-}
