{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE TypeOperators  #-}

-- | Elliptic curves (full weierstrass form) over (finite) groups.
-- This is a copy-paste of Lib.Elliptic, yes. Need a way to make things better...

module Lib.EllipticFull
       (
         ECParams(..)
       , discriminant
       , checkECParams
       , HasECParams
       , withECParams
       , ecParams

       , EC (..)
       , ECZ
       , onCurve
       , listAllPointsSlow
       , ecGroupSize
       , ecOrder
       ) where

import Universum hiding ((<*>))

import Data.Reflection (Given (..), give)

import Lib.Field

data ECParams f = ECParams
    { ecA1 :: f
    , ecA2 :: f
    , ecA3 :: f
    , ecA4 :: f
    , ecA6 :: f
    }

discriminant :: Ring f => ECParams f -> f
discriminant ECParams{..} = do
    let b2 = ecA1 <*> ecA1 <+> 4 `times` ecA2
    let b4 = 2 `times` ecA4 <+> ecA1 <*> ecA3
    let b6 = ecA3 <^> 2 <+> 4 `times` ecA6
    let b8 = (ecA1 <^> 2) <*> ecA6 <+>
             (4 `times` (ecA2 <*> ecA6)) <->
             ecA1 <*> ecA3 <*> ecA4 <+>
             ecA2 <*> (ecA3 <^> 2) <->
             ecA4 <^> 2
    9 `times` b2 <*> b4 <*> b6 <->
        (b2 <^> 2) <*> b8 <->
        8 `times` (b4 <^> 2) <->
        27 `times` (b6 <^> 2)

checkECParams :: Ring f => ECParams f -> Bool
checkECParams p = discriminant p /= f0

type HasECParams f = Given (ECParams f)

withECParams :: (Ring f) => ECParams f -> (HasECParams f => r) -> r
withECParams p f = if checkECParams p then give p f else error "wrong params"

ecParams :: HasECParams f => ECParams f
ecParams = given

-- | A point on the elliptic curve over a field f.
data EC f = EC f f | EC0 deriving (Eq,Show)

-- | EC over finite field of integers.
type ECZ n = EC (Z n)

onCurve :: (HasECParams f, Ring f) => EC f -> Bool
onCurve EC0      = True
onCurve (EC x y) =
    let ECParams{..} = ecParams
    in y <^> 2 <+> ecA1 <*> x <*> y <+> ecA3 <*> y ==
       x <^> 3 <+> ecA2 <*> (x <^> 2) <+> ecA4 <*> x <+> ecA6

neg :: (HasECParams f, Field f) => EC f -> EC f
neg EC0      = EC0
neg (EC x y) = let ECParams{..} = ecParams in EC x (fneg y <-> ecA1 <*> x <-> ecA3)

ecPlus :: forall f. (HasECParams f, Field f) => EC f -> EC f -> EC f
ecPlus EC0 x = x
ecPlus x EC0 = x
ecPlus p1@(EC x1 y1) p2@(EC x2 y2)
    | neg p1 == p2 = EC0
    | otherwise = EC x3 y3
  where
    (x3 :: f) = λ <^> 2 <+> ecA1 <*> λ <-> ecA2 <-> x1 <-> x2
    (y3 :: f) = fneg (λ <+> ecA1) <*> x3 <-> ν <-> ecA3
    denom = finv (2 `times` y1 <+> ecA1 <*> x1 <+> ecA3)
    λ = if p1 == p2
        then (3 `times` x1 <^> 2 <+> 2 `times` ecA2 <*> x1 <+> ecA4 <-> ecA1 <*> y1) <*> denom
        else (y2 <-> y1) <*> finv (x2 <-> x1)
    ν = if p1 == p2
        then (fneg (x1 <^> 3) <+> ecA4 <*> x1 <+> 2 `times` ecA6 <-> ecA3 <*> y1) <*> denom
        else (y1 <*> x2 <-> x1 <*> y2) <*> finv (x2 <-> x1)
    ECParams {..} = ecParams @f

instance (Field f, HasECParams f) => AGroup (EC f) where
    f0 = EC0
    (<+>) = ecPlus
    fneg = neg

-- | This is quadratic. I'm not sure how to simply implement it in
-- linear time.
listAllPointsSlow :: forall f. (Ord f, HasECParams f, FField f) => [EC f]
listAllPointsSlow = EC0 : filter onCurve [EC x y | x <- allElems, y <- allElems]

-- This function is slow.
ecGroupSize :: forall f. (Ord f, HasECParams f, FField f) => Integer
ecGroupSize = fromIntegral $ length $ listAllPointsSlow @f

ecOrder :: forall f. (Ord f, HasECParams f, FField f) => EC f -> Integer
ecOrder p0 = go 1 p0
  where
    go i p | p == f0 = i
           | otherwise = go (i+1) (p <+> p0)
