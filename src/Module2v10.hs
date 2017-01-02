{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Module2v10 where

import           Control.Lens   (ix, (%~), (&), _1)
import           Data.Bifunctor (bimap)
import           Data.Bool      (bool)
import           Data.Foldable  (foldl')
import Data.List (intercalate)
import           Data.Proxy     (Proxy (..))
import           Debug.Trace
import           Prelude        hiding ((<*>))

import           Lib            (inverse)

----------------------------------------------------------------------------
-- 2.29
----------------------------------------------------------------------------

{-
This one is solved by observing that map φ: R → R is, in fact,
injective. And since domain and range sizes are the same, it's also
surjective and bijective. So then we conclude that there should exist
such b that φ(b) = 1 because of bijectivity. Then φ(b) = a∙b = 1. If
we're in commutative ring. then it's enough to conclude that b is
multiplicative inverse of a. Otherwise let's consider φ_l and φ_r,
multiplicating by a from left and right sides correspondingly. The
proof is the same.
-}

----------------------------------------------------------------------------
-- 2.30
----------------------------------------------------------------------------

{-
So R is a ring.

(a): let's suppose 0, 0' exist. Then 0' ={axiom}= 0' + 0 = 0.
(b): 1' = 1 * 1' = 1.
(c): b' = -a, b = -a. b + a = 0 = b' + a. Now subtract any additive
     inverse of a from right side to get b + 0 = b' + 0 => b = b'.
(d): Let's take a ≠ 0. a∙0 = a∙(0+0) = a∙0 + a∙0. Subtract a∙0 => a∙0 = 0.
(e): -(-a) + (-a) = 0. -(-a) + (-a) + a = a. -(-a) + 0 = a. -(-a) = a.
(f): (-1)(-1) = ((-1) + (1 + (-1)))(-1) = (-1)(-1) + 1∙(-1) + (-1)(-1).
     0 = (-1)(-1) + (-1)
     1 = (-1)(-1)
(g): ∀b≠0 b|0. Well, b|0 means ∃k | b∙k = 0. This k is 0 -- (d).
(h): a ∈ R. If there's no mult. inverse, then number of inverses is 0 ≤ 1.
     Otherwise let b and c = a⁻¹ both inverses. Then ac = ab = 1.
     cac = cab ⇒ (ca)c = (ca)b ⇒ c = b.
-}

----------------------------------------------------------------------------
-- 2.31
----------------------------------------------------------------------------

{-
a. let φ(0r) = b ≠ 0s. But φ is homomorihism. φ(0r) = φ(0r+0r) = b+b = b.
   Then b = 0.

   b = φ(a) = φ(a∙1r) = b*φ(1r). Same for left side ⇒ by axiom φ(1r) = 1s.

   φ(a+(-a)) = φ(a)+φ(-a) = 0. φ(a) = -φ(-a).

   φ(a⁻¹∙a) = φ(a)φ(a⁻¹) = 1. Then φ(a⁻¹)⁻¹ = φ(a) by the definition.

b. φ(a) = a^p, where pa = 0 for every a ∈ R.
   Let's first check φ(a∙b) = φ(a)∙φ(b). φ(a∙b) = (a∙b)^p = (a∙b)∙(a∙b)∙...
   Without commutativity we can't rearrange elements so let's assume
   "ring" means "commutative ring" here. Then this rule is obviously true,
   even without using "pa = 0".

   Second thing is φ(a+b) = (a+b)^p. Let's use binomial theorem, then
   (a+b)^p = a^p + b^p + \sum{i=1..p-1}(\binom{p,k}∙a^k∙b^(p-k)). Let's
   notice that \binom{p,k} = p!/(k! * (p-k)!), so it's essentially p*s
   (here i use '*' instead of '∙' because it's integer arithmetics). Then
   this sum is something like \sum{..}{p*s a^k∙b^(p-k)}, equal to zero, because
   a^k∙b^(p-k) ∈ R, p(..) = 0 by assumption and s times 0 = 0.
-}

----------------------------------------------------------------------------
-- 2.32
----------------------------------------------------------------------------

{-
R - ring, m ∈ R, m ≠ 0.

a₁ ≡ a₂ (mod m), b₁ ≡ b₂ (mod m).

* a₁ +- a₂ ≡ b₁ +- b₂ (mod m). We just take the definition:
  a₁ - a₂ = k∙m.
  b₁ - b₂ = l∙m.
  Then summing them up or subtracting and rearranging right part using
  distribution rule we will get the result.
* a₁∙b₁ - a₂∙b₂ =
  a₁∙(b₂ + s∙m) - (a₁ - c∙m)b₂ =
  a₁∙s∙m + b₂∙c∙m = k∙m
-}

----------------------------------------------------------------------------
-- 2.33
----------------------------------------------------------------------------

{-
R is ring. Let a^ stay for equivalence class of a.
a^+b^ = (a+b)^. Obviously, it's true because of 2.32. Same for a^∙b^ = (a∙b)^.

And then we go proving ring axioms. Regarding addition:
* associativity: a^+(b^+c^) = a^+(b+c)^ = (a+(b+c))^ = rearrange and back
* commutativity: same
* id element: 0^ + a^ = (0+a)^ = a^

Same regarding multiplication.
-}

----------------------------------------------------------------------------
-- 2.34
----------------------------------------------------------------------------

{-
F -- field. a,b -- nonzero polynomials in F[x].
(a) deg(a∙b) = deg(a)∙deg(b). Leading coefficients α and β have multiplicative
    inverses, so α∙β ≠ 0 ∈ F. And clearly highest powers of x will multiply and
    get us what we want.
(b) Let c = a⁻¹. Notice that since deg(a∙b) = deg(a)*deg(b), for any a*c = 1,
    we must have deg(a) + deg(c) = deg(1) = 0. So deg(a) = deg(c) = 0. In other
    words, both a and c should be constant polynomials.
(c) a ∈ F[x], a ≠ 0. a = p₁...pₙ, pᵢ -- irreducable. By induction on polynomial
    degree -- starting with 1, because polynomials of degree 0 are units and
    so are not irreducable by definition.
    * Base: deg(a) = 1, a = αx + β. It's definitely not unit, because only
      constant polynomials are units. It also doesn't have non-trivial factors
      (of degree > 0), so it's irreducable.
    * Step: deg(a) = n. If a doesn't have non-trivial factors, it's irreducable
      by definition. If it does, a = b∙c. But deg(a) > deg(b) + deg(c) and so
      a is not a unit and a factors in p₁...pₙ∙q₁...qₘ (from b,c) that has
      units by induction case.
(d) R = Z/6Z. (3x²+2)(2x²+1) = 6x² + 7x² + 2 = x² + 2. Degrees does not match
    because R is not a field, so 3 doesn't have mult. inverse and thus 3 and 2
    are zero divisors.
-}

----------------------------------------------------------------------------
-- 2.35
----------------------------------------------------------------------------

class WithTag a where
    getTag :: Num n => Proxy a -> n

class Ring a where
    f0 :: a
    (<+>) :: a -> a -> a
    fneg :: a -> a
    f1 :: a
    (<*>) :: a -> a -> a

(<->) :: Ring a => a -> a -> a
(<->) a b = a <+> (fneg b)

instance Ring Integer where
    f0 = 0
    f1 = 1
    (<+>) = (+)
    fneg = negate
    (<*>) = (*)

class Ring a => Field a where
    finv :: a -> a
    -- ^ Multiplicative inverse

-- Z/nZ
newtype Z a = Z Integer deriving (Num, Eq, Ord, Enum, Real, Integral)

instance Show (Z a) where
    show (Z i) = show i

#define GenZ(N) \
  data Z/**/N = Z/**/N; \
  instance WithTag Z/**/N where { getTag _ = fromIntegral N };\

GenZ(2)
GenZ(3)
GenZ(4)
GenZ(5)
GenZ(6)
GenZ(7)
GenZ(8)
GenZ(9)
GenZ(11)
GenZ(13)

instance WithTag a => WithTag (Z a) where
    getTag _ = getTag (Proxy :: Proxy a)

instance WithTag a => Ring (Z a) where
    f0 = Z 0
    (Z a) <+> (Z b) = Z $ (a + b) `mod` getTag (Proxy :: Proxy (Z a))
    f1 = Z 1
    fneg (Z 0) = Z 0
    fneg (Z i) = Z $ (getTag (Proxy :: Proxy (Z a)) - i) `mod` getTag (Proxy :: Proxy (Z a))
    (Z a) <*> (Z b) = Z $ a * b `mod` getTag (Proxy :: Proxy (Z a))

instance (WithTag a) => Field (Z a) where
    finv a = inverse a (getTag (Proxy :: Proxy (Z a)))

-- Empty polynomial is equivalent for [0]. Head -- higher degree.
newtype Poly a = Poly { fromPoly :: [a] } deriving (Functor)

instance Show a => Show (Poly a) where
    show (Poly l) = "Poly " ++ show l

-- Removes zeroes from the beginning
stripZ :: (Eq a, Ring a) => Poly a -> Poly a
stripZ (Poly []) = Poly [f0]
stripZ r@(Poly [a]) = r
stripZ (Poly xs) =
    let l' = take (length xs - 1) xs
    in Poly $ dropWhile (== f0) l' ++ [last xs]

prettyPoly :: (Show a, Eq a, Ring a) => Poly a -> String
prettyPoly (stripZ -> (Poly p)) =
    intercalate " + " $
    map mapFoo $
    filter ((/= f0) . fst) $
    reverse $ reverse p `zip` [0..]
  where
    mapFoo (n,0) = show n
    mapFoo (f,i) | f == f0 = "x^" ++ show i
    mapFoo (n,i) = show n ++ "x^" ++ show i

instance (Eq a, Ring a) => Eq (Poly a) where
    (==) (stripZ -> (Poly p1)) (stripZ -> (Poly p2)) = p1 == p2

deg ::  (Eq a, Ring a, Integral n) => Poly a -> n
deg (stripZ -> (Poly p)) = fromIntegral $ length p - 1

-- Zips two lists adding zeroes to end of the shortest one
zip0 :: (Ring a) => [a] -> [a] -> [(a,a)]
zip0 p1 p2 = uncurry zip sameSize
  where
    shortest | length p1 < length p2 = (p1,p2)
             | otherwise = (p2,p1)
    diff = length (snd shortest) - length (fst shortest)
    sameSize = shortest & _1 %~ ((replicate diff f0) ++)

instance (Eq a, Ring a) => Ring (Poly a) where
    f0 = Poly [f0]
    f1 = Poly [f1]
    fneg = fmap fneg
    (Poly p1) <+> (Poly p2) =
        stripZ $ Poly $ map (uncurry (<+>)) $ zip0 p1 p2
    lhs@(Poly p1) <*> rhs@(Poly p2) =
        let acc0 = replicate ((deg lhs + deg rhs)+1) f0
            foldFooSub acc ((e1,d1), (e2,d2)) =
                acc & ix (d1 + d2) %~ (<+> (e1 <*> e2))
            foldFoo acc ((e1,d1),el2) =
                foldl' foldFooSub acc $ map ((e1,d1),) $ withIndex el2
            withIndex a = reverse $ reverse a `zip` [0..]
        in stripZ . Poly $ reverse $ foldl' foldFoo acc0 $ map (,p2) $ withIndex p1

class Ring a => Euclidian a where
    (</>) :: a -> a -> (a,a)
    -- ^ Division with (quotient,remainder)

instance Euclidian Integer where
    (</>) a b = (a `div` b, a `mod` b)

-- Ugh, terrible
instance WithTag a => Euclidian (Z a) where
    (</>) (Z a) (Z b) =
        bimap
            (Z . (`mod` getTag (Proxy :: Proxy (Z a))))
            (Z . (`mod` getTag (Proxy :: Proxy (Z a))))
            (a `div` b, a `mod` b)

assert bool str action
    | not bool = error str
    | otherwise = action

-- | a / b = (quotient,remainder)
euclPoly :: (Eq a, Field a) => Poly a -> Poly a -> (Poly a, Poly a)
euclPoly (stripZ -> a@(Poly p1)) (stripZ -> b@(Poly p2)) =
    let res@(q,r) = euclPolyGo f0 a
    in assert ((b <*> q) <+> r == a) "EuclPoly assert failed" res
  where
    euclPolyGo (stripZ -> k) (stripZ -> r)
        | deg r < deg b || r == f0 = (k,r)
    euclPolyGo (stripZ -> k) (stripZ -> r@(Poly pr)) =
        let e = deg r
            d = deg b
            re = pr !! 0
            bd = p2 !! 0
            x = Poly $ (re <*> (finv bd)) : replicate (e - d) f0
            k' = k <+> x
            r' = r <-> (x <*> b)
        in euclPolyGo k' r'

instance (Field a, Eq a) => Euclidian (Poly a) where
    (</>) = euclPoly

gcdEucl :: (Eq a, Euclidian a) => a -> a -> a
gcdEucl a b =
    let res = gcdEuclGo a b
    in assert (snd (a </> res) == f0) "gcd doesn't divide a" $
       assert (snd (b </> res) == f0) "gcd doesn't divide a" $
       res
  where
    gcdEuclGo r0 r1 =
        let (k,r) = r0 </> r1
        in if r == f0 then r1 else gcdEuclGo r1 r

{-
λ> gcdEucl (Poly [1,0,2,10::Z Z13]) (Poly [3,4,6])
Poly [9,4]
-}

e235a, e235b :: (WithTag a) => Poly (Z a)
e235a = f0 <+> Poly [1, 3, fneg 5, fneg 3, 2, 2]
e235b = f0 <+> Poly [1, 1, fneg 2, 4, 1, 5]

e235gcd :: forall a. (WithTag a) => Poly (Z a)
e235gcd = gcdEucl e235a e235b

e235 = do
    putStrLn . prettyPoly $ e235gcd @Z2
    putStrLn . prettyPoly $ e235gcd @Z3
    putStrLn . prettyPoly $ e235gcd @Z5
    putStrLn . prettyPoly $ e235gcd @Z7

{-
λ> e235
1x^3 + 1x^2 + 1x^1 + 1
1x^2 + 2x^1 + 2
4x^1 + 1
4
-}
