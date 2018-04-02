{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Elliptic curves over R.

module Module6v1 () where

import Universum

import Lib.Elliptic
import Lib.Field

----------------------------------------------------------------------------
-- 6.1
----------------------------------------------------------------------------

e61 :: IO ()
e61 = withECParams (ECParams (-2) 4 :: ECParams Double) $ do
    let (p :: EC Double) = EC 0 2
    let (q :: EC Double) = EC 3 (-5)
    print $ p <+> q
    print $ p <+> p
    print $ q <+> q
    print $ 3 `times` p
    print $ 3 `times` q

{-
λ> ex61
EC 2.444444444444443 3.7037037037036997
EC 0.25 (-1.875)
EC 0.25 (-1.875)
EC 240.0 3718.0
EC (-1.958677685950413) (-0.6348610067618328)
-}

----------------------------------------------------------------------------
-- 6.2
----------------------------------------------------------------------------

e62 :: IO ()
e62 = withECParams (ECParams 0 17 :: ECParams Double) $ do
    let (p :: EC Double) = EC (-1) 4
    let (q :: EC Double) = EC 2 5
    print $ onCurve p
    print $ onCurve q
    print $ p <+> q
    print $ p <-> q
    print $ 2 `times` p
    print $ 2 `times` q
    putText "bonus: 4 pairs of solutions"

{-
λ> e62
True
True
EC (-0.8888888888888888) (-4.037037037037037)
EC 8.0 23.0
EC 2.140625 (-5.177734375)
EC (-2.5599999999999996) 0.4720000000000004
bonus: 4 pairs of solutions
-}

----------------------------------------------------------------------------
-- 6.3
----------------------------------------------------------------------------

{-
x^3 - (e1+e2+e3)x^2 + (e1e2 + e2e3 + e1e3)x - e1e2e3

e1 + e2 + e3 = 0 must hold             (1)

So A = (e1e2 + e2e3 + e1e3), B = e1e2e3


(⇒)
Let's prove that if >2 are equal, then D = 0.

Let e1 = e2 = a, e3 = b. A = (a^2 + 2ab), B = ba^2

Then 4A^3 + 27B^2 has only one integer root: a = 0, b = 0 and (1) is false.

(⇐)
4A^3 + 27B^2 = 0 iff e1 = 0, e2 = 0

-}

----------------------------------------------------------------------------
-- 6.4
----------------------------------------------------------------------------

{-
http://www.wolframalpha.com/input/?i=plot+y%5E2+%3D+x%5E3+-+7x+%2B+3
http://www.wolframalpha.com/input/?i=plot+y%5E2+%3D+x%5E3+-+7x+%2B+9
http://www.wolframalpha.com/input/?i=plot+y%5E2+%3D+x%5E3+-+7x+-12
http://www.wolframalpha.com/input/?i=plot+y%5E2+%3D+x%5E3+-+3x+%2B+2
http://www.wolframalpha.com/input/?i=plot+y%5E2+%3D+x%5E3
-}
