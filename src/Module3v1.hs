-- | Section 3.1 solutions.

module Module3v1 where

import           Universum           hiding (exp)

import           Data.List           (nub)
import           Data.Numbers.Primes (primeFactors)

import           Lib                 (eulerPhiSlow, exp, inverse)

----------------------------------------------------------------------------
-- 3.1
----------------------------------------------------------------------------

bruteForce31 :: Integer -> Integer -> Integer -> Maybe Integer
bruteForce31 e c p = do
    head $ filter (\x -> x^e `mod` p == c) [1..p-1]

{-

1-4)
λ> bruteForce31 19 36 97
Just 36
λ> bruteForce31 137 428 541
Just 213
λ> bruteForce31 73 614 1159
Just 158
λ> bruteForce31 751 677 8023
Just 1355

5)
λ> inverse 38993 (606*660)
265457
λ> exp 401227 328047 265457
36219
λ> exp 401227 36219 38993
328047 -- indeed

-}

----------------------------------------------------------------------------
-- 3.2
----------------------------------------------------------------------------

{-
x^e = c (mod p). Let s be solution. Let's look for solution in form ks.

Then ks^e = c (mod p) ⇒ k^e = 1 (mod p). Since F_p has generator g,
let k = g^t. So g^{et} = 1 (mod p) which leads to et = 0 (mod p-1).

Let gcd(e,p-1) = ψ, then lcm(e,p-1) = e∙(p-1)/ψ.

Let's take t = i(p-1)/ψ. Then g^(ie(p-1)/ψ) = 0 (mod (p-1)).

And all {g^{i(p-1)/ψ}} are different, and i ∈ {0..ψ-1}.
-}

----------------------------------------------------------------------------
-- 3.3
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- 3.4 Euler's phi basic properties
----------------------------------------------------------------------------


{-
(a)

λ> eulerPhi 6
2
λ> eulerPhi 9
6
λ> eulerPhi 15
8
λ> eulerPhi 17
16

(b) p-1 (obviously)

(c) Following the proof of Fermat's little theorem.

Let's consider {ka | gcd(k,N) = 1}. All elements are different. If
not, then ja = ia (mod N), then (j-i)a = 0 (mod N). N can't divide a
since gcd(a,N)=1, then N divides (j-i), but this difference Δ < |p-2|,
then j-i=0. Since this set is same as just {k} but in the different order,

We then rearrange and get a^{p-1} = 1.

-}

----------------------------------------------------------------------------
-- 3.5 More properties
----------------------------------------------------------------------------

{-
1. φ(pq) = (p-1)(q-1) = φ(p)φ(q).
   k ∈ φ(pq) if gcd(k,pq) = 1.
   ∀s ∈ φ(p), s ∈ φ(pq), since only p and q divide pq. Same for φ(q).
   So we have at least max(p-1, q-1) elements in φ(pq).
   Consider st (s∈φ(p),t∈φ(p)): gcd(st,pq) is still 1 since pq must divide either s or t,
   but it doesn't divide any.

2. φ(p^i) = (p-1)*p^(i-1).
   When you first multiply p by p, you get p numbers of kind p*i, where i ∈ [0..p-1].
   Each n iteration round multiplies this p numbers on p.
   So in order to get the value, we do p^i - p^(i-1) = (p-1)*p^(i-1).

   E.g. for 25 it's 5 (0,5,10,15,20), for 25^5=125 it's 5*5 = 25,
   for 125^5 it's 25*5=125 and so on.

3. It's the same as in (1). We have set M' = (Z/MZ)* and N'. Only m'∙n' ∈ MN' because:
   M'' is set of numbers that divide M, N'' -- that divide N. Then what divides NM?
   1. All elements of M'' and N'' plus M and N themselves.
   2. Such m''n'' that m''∈M'', n''∈N''. If only one element of multiplication was
      dividing either M or N, then product won't divide MN.

   It's easy to see that M' ⊔ M'' = (Z/MZ) and that M'×N' ⊔ M''×N'' = (Z/(MN)Z).
   Because it's disjoint, M'×N' ≡ MN'.

4. N = (N/p1...pr) * p1...pr = (N/p1...pr) * p1(1-1/p1) * p2(1-1/p2) * ... =
     = N * (1-1/p1) (1-1/p2) ...
-}

eulerCheck35 :: Int -> Int -> Bool
eulerCheck35 x n = eulerPhiSlow (x ^ n) == (x ^ (n-1)) * (x-1)

-- | Fast Euler calculation function using Euler's formula.
eulerFast :: Int -> Int
eulerFast n =
    round $
    toRational n *
    product (map (\x -> 1 - 1 / (toRational x)) $ nub $ primeFactors n)

{-
5.

λ> eulerFast 1728
576
λ> eulerFast 1575
720
λ> eulerFast 889056
254016

-}

----------------------------------------------------------------------------
-- 3.6
----------------------------------------------------------------------------

{-
gcd(N,c) = 1
gcd(e,φ(N)) = 1

x^e = c (mod N)

We know that if we find such d that de ≡ 1 (mod p-1)
then x = c^d (mod N) solves the equation. Let's show it again for non-prime N.

So gcd(e,φ(N)) = 1, then let d ≡ e^(-1) mod φ(N), which means de = 1 mod φ(N).

Let's check c^d: (c^d)^e = c^de = c^(1 + k * φ(N)) = c * (c^φ(N))^k = c (all modulo N).
-}

solve36 :: Int -> Int -> Int -> Int
solve36 e c n = exp n c (inverse e (eulerFast n))

{-
λ> solve36 577 60 1463
1390
λ> solve36 959 1583 1625
147
λ> solve36 133957 224689 2134440
1892929
-}
