{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE TypeOperators  #-}

-- | Elliptic curves over finite groups.

module Lib.Elliptic
       (
         ECParams(..)
       , checkECParams
       , HasECParams
       , withECParams
       , ecParams

       , EC (..)
       , onCurve
       ) where

import Universum hiding ((<*>))

import Data.Reflection (Given (..), give)

import Lib.Field

-- Reflection-based approach is terrible, but i can't promote Doubles.

-- It is assumed that params satisfy the discriminant rule.
data ECParams f = ECParams { ecA :: f, ecB :: f }

checkECParams :: Ring f => ECParams f -> Bool
checkECParams ECParams{..} =
    (4 :: Int) `times` (ecA <^> (3 :: Int)) <+>
    (27 :: Int) `times` (ecB <^> (2 :: Int))
    /= f0

type HasECParams f = Given (ECParams f)

withECParams :: (Ring f) => ECParams f -> (HasECParams f => r) -> r
withECParams p f = if checkECParams p then give p f else error "wrong params"

ecParams :: HasECParams f => ECParams f
ecParams = given


-- | A point on the elliptic curve y^2 = x^2 + ax + b over a finite
-- field x.
data EC f = EC f f | EC0 deriving (Eq,Show)

onCurve :: (HasECParams f, Ring f) => EC f -> Bool
onCurve EC0      = False
onCurve (EC x y) =
    let ECParams{..} = ecParams
    in y <^> (2 :: Int) == x <^> (3 :: Int) <+> ecA <*> x <+> ecB

ecPlus :: forall f. (HasECParams f, Field f) => EC f -> EC f -> EC f
ecPlus EC0 x = x
ecPlus x EC0 = x
ecPlus p1@(EC x1 y1) p2@(EC x2 y2)
    | x1 == x2 && y1 == (fneg y2) = EC0
    | otherwise = EC x3 y3
  where
    tm (x :: Int) = times x
    (x3 :: f) = λ <^> (2::Int) <-> x1 <-> x2
    (y3 :: f) = λ <*> (x1 <-> x3) <-> y1
    λ = if p1 == p2 then (3 `tm` (x1 <^> (2::Int)) <+> ecA) <*> finv (2 `tm` y1)
                    else (y2 <-> y1) <*> finv (x2 <-> x1)
    ECParams {..} = ecParams

instance (Field f, HasECParams f) => AGroup (EC f) where
    f0 = EC0
    (<+>) = ecPlus
    fneg = \case EC0 -> EC0
                 (EC a b) -> EC a (fneg b)
