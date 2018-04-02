{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Lenstra's factorization algo.

module Module6v6 where

import Universum hiding ((<*>))

import Lib.Elliptic
import Lib.Field
import Lib.Misc

import Module6v3 (ternExpand)

ecPlus ::
       forall n. (HasECParams (Z n), KnownNat n)
    => ECZ n
    -> ECZ n
    -> Either (Z n) (ECZ n)
ecPlus EC0 x = Right x
ecPlus x EC0 = Right x
ecPlus p1@(EC x1 y1) p2@(EC x2 y2)
    | x1 == x2 && y1 == (fneg y2) = Right EC0
    | otherwise = do
      let ECParams {..} = ecParams
      let inv :: Z n -> Either (Z n) (Z n)
          inv (Z i) = let m = fromIntegral (natVal (Proxy @n))
                          (gcd',u,_) = exEucl i m
                      in if gcd' == 1
                         then Right (Z $ u `mod` m)
                         else Left (Z gcd')
      λ <- if p1 == p2
           then (<*>) (3 `times` (x1 <^> 2) <+> ecA) <$> inv (2 `times` y1)
           else (<*>) (y2 <-> y1) <$> inv (x2 <-> x1)
      let (x3 :: (Z n)) = λ <^> 2 <-> x1 <-> x2
      let (y3 :: (Z n)) = λ <*> (x1 <-> x3) <-> y1
      pure $ EC x3 y3

-- Either-based version of 'times'.
ectimes ::
       forall n. (HasECParams (Z n), KnownNat n)
    => Integer
    -> ECZ n
    -> Either (Z n) (ECZ n)
ectimes n0 p0 =
    second snd $ foldl foldFoo (Right (p0,EC0)) (ternExpand n0)
  where
    neg :: ECZ n -> ECZ n
    neg = \case EC0 -> EC0
                (EC a b) -> EC a (fneg b)
    foldFoo :: Either (Z n) (ECZ n, ECZ n) -> Integer -> Either (Z n) (ECZ n, ECZ n)
    foldFoo acc i = do
        (p,v) <- acc
        e1 <- p `ecPlus` p
        e2 <- case i of
                  0    -> pure v
                  1    -> v `ecPlus` p
                  (-1) -> v `ecPlus` (neg p)
                  _    -> error "ectimes"
        pure (e1,e2)

lenstraFactor :: forall n. KnownNat n => Integer -> (Integer, Integer) -> Maybe Integer
lenstraFactor aBig (a, b) =
    let n' = natVal (Proxy @n)
        bBig = (b*b  - a*a*a - aBig * a) `mod` n'
    in withECParams (ECParams (toZ aBig) (toZ bBig) :: ECParams (Z n)) $
       let (p0 :: ECZ n) = EC (toZ a) (toZ b)
           loop p j | j > 200 = Nothing
                    | otherwise = case j `ectimes` p of
                                      Left (Z d) -> if d < n' then Just d else Nothing
                                      Right q    -> loop q (j+1)
       in loop p0 2

e621 :: IO ()
e621 = do
    print $ lenstraFactor @589 4 (2,5)
    print $ lenstraFactor @26167 4 (2,12)
    print $ lenstraFactor @1386493 3 (1,1)
    print $ lenstraFactor @28102844557 18 (7,4)

{-

λ> e621
Just 19
Just 191
Just 1297
Just 117763

-}
