-- | Counting principles. Mostly skipped (trivial).

module Module5v1 () where

----------------------------------------------------------------------------
-- 5.1
----------------------------------------------------------------------------

{-
7 + 7^2 + 7^3 + 7^4 + 7^5

Prelude> sum $ map (\x -> 7 ^ x) [1..5]
19607
-}

----------------------------------------------------------------------------
-- 5.2
----------------------------------------------------------------------------

{-
Boring.

(d) We need to count increasing sequences, which is
the same as setting (q-1) separators in n elements, which
is the same as representing n as sum of q numbers.

Voila: (n + q - 1 choose q - 1)

(e) 12*(1+2*(1+100*(1+32))) = 79236

-}

----------------------------------------------------------------------------
-- 5.3
----------------------------------------------------------------------------

{-
Boring.

(e) 4! / 2! = 12.
-}

----------------------------------------------------------------------------
-- 5.4
----------------------------------------------------------------------------

{-
The general formula for permutations with repetitions is that if you have
n elements, where some elements can repeat (say, k types, sum(k_i) = n)

(a) 4! / (2! * 2!) = 6
(b) 7! / (4! * 3!) = 35
(c) 9! / (4! * 3! * 2!) = 1260
(d) 5! / (2! * 3!) +      -- AABBB
    5! / (3! * 2!) +      -- AAABB
    5! / (4! * 1!) =      -- AAAAB
    25
-}

----------------------------------------------------------------------------
-- 5.5
----------------------------------------------------------------------------

{-
(a) 100 * 5
(b) 100 * 99 * 98 ≡ l
(c) (100 * n1) * (99 * n2) * (98 * n3) = l * n^3
(d) (100 choose 3) * n^3

prod{i=0..k}(n-i) = (n choose k+1) * (k+1)!, so
  (100 choose 3) = l * 3!

d > c > b > a
-}

----------------------------------------------------------------------------
-- 5.7
----------------------------------------------------------------------------

{-
(a)(b) b o r i n g

(c) Ok, so we have a1...an and we'd like to choose j elements. Let's say we
are allowed to choose j from n-1 only, leaving some element X, so we get
this (n-1 choose j). What is left to add? Obviously, element X. To what will
we add it? To binomial coefficients of size j-1, from the very same n-1 set.
-}

----------------------------------------------------------------------------
-- 5.8 Fermat's lemma again
----------------------------------------------------------------------------

{-
(a) Using formula from 5.7:

   (n choose j)
     = (n-1 choose j-1) + (n - 1 choose j)
     = (n-2 choose j-2) + 2 (n-2 choose j-1) + (n-2 choose j)
     = (n-3 choose j-3) + 3 (n-3 choose j-2) + 3 (n-3 choose j-1) + (n-3 choose j)

   Hmm! Must be:

   (n choose j) = sum{i=0..(k-1)}(n-k choose j-i)
   but the upper bound doesn't make sense if i > j, so
   max k is (j+1).

Ok, then let's take k = n-j, so we'll have

   (n choose j) = sum{i=0..(n-j-1)}(j choose j-i)
                = sum{i=0..(n-j-1)}(j!/[(j-i)!i!])

, which is the sum of j * smth, so it j divides it.

Ok, what to do when k = n-j > j+1 (j < (n-1)/2)? Good news, binomial
coefficients are symmetrical: (n choose j) = (n choose n-j), so
if j < (n-1)/2, just take j' = n-j, which is greater than (n-1)/2.

(b) Straightforward, all elements except for the first/last one are 0.

(c) (n + 1)^p = n^p + 1^p = n + 1 (mod p).

(d) Just divide by a.

-}

----------------------------------------------------------------------------
-- 5.9 Derangements and recontres numbers
----------------------------------------------------------------------------

{-
(a) Ok, so the proof is slightly complicated IMHO, but i've managed to
    do something like this:
    https://neerc.ifmo.ru/wiki/index.php?title=%D0%A4%D0%BE%D1%80%D0%BC%D1%83%D0%BB%D0%B0_%D0%B2%D0%BA%D0%BB%D1%8E%D1%87%D0%B5%D0%BD%D0%B8%D1%8F-%D0%B8%D1%81%D0%BA%D0%BB%D1%8E%D1%87%D0%B5%D0%BD%D0%B8%D1%8F#.D0.91.D0.B5.D1.81.D0.BF.D0.BE.D1.80.D1.8F.D0.B4.D0.BA.D0.B8

    The answer is S = n! + sum{(-1)^i * n!/i!}

(b) V = n! - S = sum{(-1)^(i+1) * n!/i!}

(c) Exactly k numbers fixed: fix these k numbers (n choose k) and for each
    compute derangement of (n-k)

    Result is N_{n,k} = n!/k! * sum{(-1)^i / i!}

(d) V - N_{n,1}
-}
