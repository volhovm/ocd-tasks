{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Applications of Weil pairing.

module Module6v10 () where

import Universum hiding ((<*>))

import Lib.Elliptic
import Lib.Field

----------------------------------------------------------------------------
-- 6.47 Tripartite D-H
----------------------------------------------------------------------------

data TdhParams f = TdhParams
    { tdhL   :: Integer
    , tdhPhi :: EC f -> EC f
    , tdhP   :: EC f
    }

newtype TdhSec = TdhSec Integer deriving (Eq,Show)
newtype TdhPub f = TdhPub (EC f) deriving (Eq,Show)

tdhToPublic :: (HasECParams f, Field f) => TdhParams f -> TdhSec -> TdhPub f
tdhToPublic TdhParams{..} (TdhSec sec) = TdhPub $ sec `times` tdhP

tdhGetSecret :: (HasECParams f, FField f) => TdhParams f -> TdhSec -> TdhPub f -> TdhPub f -> f
tdhGetSecret TdhParams{..} (TdhSec sec) (TdhPub p1) (TdhPub p2) =
    let q = tdhPhi p2
        res = millerWeil tdhL p1 q (3 `times` p1 <+> 4 `times` q)
    in res <^> sec

{-
I am going to solve this exercise as it was done in the previous
example -- by moving to a E(F_{1723^2}), which gives me the very same
φ because 1723 is 3 mod 4. It is not obvious why is this _the only_
way suggested though.
-}

type F647 = FinPolyZ 2968730 1723

e647 :: IO ()
e647 = do
    -- It's 5 am, I'm in the plane. п/10 is how I feel atm.
    let п x = mkFinPoly $ Poly [x]
    let ec x y = EC (п x) (п y)
    withECParams (ECParams (п 1) (п 0) :: ECParams F647) $ do
        let φ :: EC F647 -> EC F647
            φ EC0      = EC0
            φ (EC x y) = EC (fneg x) (y <*> mkFinPoly (Poly [1,0]))
        let p = (ec 668 995)
        let params = TdhParams 431 φ p

        let aliceSec = TdhSec 278
        let alicePub = tdhToPublic params aliceSec
        print alicePub

        let bobPub = TdhPub (ec 1275 1550)
        let carlPub = TdhPub (ec 897 1323)
        let aliceCommon = tdhGetSecret params aliceSec bobPub carlPub
        print aliceCommon

        let bobSec = TdhSec 224
        let bobCommon = tdhGetSecret params bobSec alicePub carlPub
        print bobCommon

        let Just carlSk = find (\i -> TdhPub (i `times` p) == carlPub) [0..431]
        print carlSk

{-
λ> e647
TdhPub (EC FinPoly [726] FinPoly [1127])
FinPoly [428,68]
FinPoly [428,68]
145
-}

----------------------------------------------------------------------------
-- 6.48 Breaking Tripartite DH
----------------------------------------------------------------------------

{-
Let e denote the modified Weil pairing. If there's an algorithm that
can solve DH, then pass e(P1,P2), e(P,P3) to it. Since e(P1,P2) =
e(P,P)^{s1s2} and e(P,P3) = e(P,P)^{s3}, algorithm will return
e(P,P)^{s1s2s3} which is exactly the shared secret.
-}

----------------------------------------------------------------------------
-- 6.49 Identity-based encryption
----------------------------------------------------------------------------

{-
(a) If Eve can solve ECDLP, then she retrieves s directly from P^{Tom}
    = sP, because P is known. If she can break DLP in F_q, she can
    still solve DLP for e(P^{Tom},P) = e(P,P)^r.

(b) If eve can only solve ECDHP, then she can take C1 = rP and P^{Tom} = sP,
    forge rsP and pass it to e(P^{Alice},rsP) = e(Q^{Alice},C1) that Alice
    uses to decrypt messages.

(c) Yes. Combine e(P^{Alice},rP) = G^r and e(P^{Alice},sP) = G^s to get
    G^sr = e(P^Alice,rsP).
-}
