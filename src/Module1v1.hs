{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Module1v1 () where

import Data.Char (isAlpha, toLower, toUpper)
import Prelude

upperL, lowerL :: Int
upperL = 65
lowerL = 97

enc, dec :: Int -> Char -> Char
enc rotation c = toEnum $ (fromEnum c - lowerL + rotation) `mod` 26 + upperL
dec rotation c = toEnum $ (fromEnum c - upperL - rotation) `mod` 26 + lowerL

encryptWheel :: Int -> String -> String
encryptWheel rotation = map (enc rotation) . filter isAlpha . map toLower

decryptWheel :: Int -> String -> String
decryptWheel rotation = map (dec rotation) . filter isAlpha . map toUpper

decryptMixed :: String -> String
decryptMixed = map (\(i,c) -> dec i c) . zip [1..] . filter isAlpha . map toUpper

t11a,t11b,t11c,t12a, t12b, t12c :: String
-- FUFLJTKMNXYTWDNXBTWYMFATQZRJTKQTLNH
t11a = encryptWheel 5 "A page of history is worth a volume of logic."
-- there are no secrets better than the secrets that everybody guesses
t11b = decryptWheel 7 "AOLYLHYLUVZLJYLAZILAALYAOHUAOLZLJYLAZAOHALCLYFIVKFNBLZZLZ"
-- when angry count ten before you speak if very angry an hundred
t11c = decryptMixed "XJHRFTNZHMZGAHIUETXZJNBWNUTRHEPOMDNBJMAUGORFAOIZOCC"
-- i think that i shall never see a billboard lovely as a tree"
t12a = decryptWheel 3 "LWKLQNWKDWLVKDOOQHYHUVHHDELOOERDUGORYHOBDVDWUHH"
-- love is not love which alters when it alteration finds
t12b = decryptWheel 9 "UXENRBWXCUXENFQRLQJUCNABFQNWRCJUCNAJCRXWORWMB"
-- in baiting a mouse trap with cheese always leave room for the mouse
t12c = decryptWheel 19 "BGUTBMBGZTFHNLXMKTIPBMAVAXXLXTEPTRLEXTOXKHHFYHKMAXFHNLX"

-- I skipped 1.3, it's boring
{- 1.4

JNRZR BNIGI BJRGZ IZLQR OTDNJ GRIHT USDKR ZZWLG OIBTM NRGJN
IJTZJ LZISJ NRSBL QVRSI ORIQT QDEKJ JNRQW GLOFN IJTZX QLFQL
WBIMJ ITQXT HHTBL KUHQL JZKMM LZRNT OBIMI EURLW BLQZJ GKBJT
QDIQS LWJNR OLGRI EZJGK ZRBGS MJLDG IMNZT OIHRK MOSOT QHIJL
QBRJN IJJNT ZFIZL WIZTO MURZM RBTRZ ZKBNN LFRVR GIZFL KUHIM
MRIGJ LJNRB GKHRT QJRUU RBJLW JNRZI TULGI EZLUK JRUST QZLUK
EURFT JNLKJ JNRXR S

The ciphertext contains 316 letters. Here is a frequency table:
R  J  I  L  Z  T  N  Q  B  G  K  U  M  O  S H W F E D X V
33 30 27 25 24 20 19 16 15 15 13 12 12 10 9 8 7 6 5 5 3 2

JN (11) NR (8) TQ (6) LW RB RZ JL (5)

JN = th
NR = he
Z = S
B = c
I = a
G = r
L = o
Q = n
T = i
M = p
O = m
D = g
S = y
W = f
X = k
E = b
U = l
K = u
H = d
F = w

these chara cters asone might readi lygue ssfor macip herth
atist osayt heyco nveya meani ngbut thenf romwh atisk nowno
fcapt ainki ddico uldno tsupp osehi mcapa bleof const ructi
ngany ofthe morea bstru secry ptogr aphsi madeu pmymi ndato
nceth atthi swaso fasim plesp ecies suchh oweve raswo uldap
peart othec rudei ntell ectof thesa ilora bsolu telyi nsolu
blewi thout theke y

these characters as one might readily guess form a cipher that is to
say they convey a meaning but then from what is known of captain kiddi
could not suppose him capable of constructing any of the more abstruse
cryptographs i made up my mind at once that this was of a simple
species such however as would appear to the crude intellect of the
sailor absolutely insoluble without the key

That was a hard one, i'd better not do that again...
-}

{- 1.5
First of all, fixed positions are not known because if they are,
then "more than one" is the same as "m of n".

I'd say that 1.5a and 1.5b(i) is n!, 1.5b(iii) and
1.5b(iv) is n!/m! (choice m n * (n-m)!)

Also, i don't see any difference between "more than m" and "exactly m",
because if we're in the first situation, the worst case scenario is still
m/n. Though we can optimize by choosing the order of those permutations
in the way permutations with possibly fixed letters are there, but
best/worst cases are n/n and m/n still, everything else is between.

-}
