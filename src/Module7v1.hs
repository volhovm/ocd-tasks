{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Congruential PKC.

module Module7v1 () where

import Universum

import Lib.Misc

----------------------------------------------------------------------------
-- 7.1
----------------------------------------------------------------------------

data CgSk = CgSk
    { csQ :: Integer
    , csF :: Integer
    , csG :: Integer
    } deriving (Eq, Show)

data CgPk = CgPk
    { cpH :: Integer
    , cpQ :: Integer
    } deriving (Eq, Show)

newtype CgM = CgM { unCgM :: Integer } deriving Show

cgToPublic :: CgSk -> CgPk
cgToPublic CgSk{..} = CgPk ((inverse csF csQ * csG) `mod` csQ) csQ

cgEnc :: CgPk -> Integer -> Integer -> CgM
cgEnc CgPk{..} r m = CgM $ (r * cpH + m) `mod` cpQ

cgDec :: CgSk -> CgM -> Integer
cgDec CgSk{..} (CgM e) = b
  where
    a = (csF * e) `mod` csQ
    b = (inverse csF csG * a) `mod` csG

e71 :: IO ()
e71 = do
    let sk = CgSk 918293817 19928 18643
    let pk = cgToPublic sk
    print pk

    let e = CgM 619168806
    print $ cgDec sk e

    let m = 10220
    let r = 19564
    print $ cgEnc pk r m

    print $ cgDec sk (cgEnc pk r m) == m

{-
λ> e71
CgPk {cpH = 767748560, cpQ = 918293817}
11818
CgM {unCgM = 619167208}
True -- sanity check
-}
