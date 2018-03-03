{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeInType     #-}
{-# LANGUAGE TypeOperators  #-}

-- | Elliptic curves over finite groups.

module Lib.Elliptic where

import Universum hiding ((<*>))

import Lib.Field

data ECParams x = ECParams { ecA :: x, ecB :: x }

class ValidEC p where
    getParams :: ECParams x

-- | A point on the elliptic curve y^2 = x^2 + ax + b over a finite
-- field x.
data EC f (p :: ECParams x) = EC f f | EC0 deriving (Eq,Show)

ecPlus :: forall f p. (ValidEC p,Field f) => EC f p -> EC f p -> EC f p
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
    ECParams {..} = getParams @p

instance (Field f, ValidEC p) => AGroup (EC f p) where
    f0 = EC0
    (<+>) = ecPlus
    fneg = \case EC0 -> EC0
                 (EC a b) -> EC (fneg a) b
