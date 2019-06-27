-- | Smth.

module Playground where

import Universum hiding (exp, (<*>))

import Data.List (last, (!!))
import Data.Numbers.Primes (isPrime, primeFactors, primes)
import Module3v4 (isPrimeMR)
import System.Random (randomRIO)

import Lib


allEvenOrders :: Integer -> Bool
allEvenOrders n = all even $ mapMaybe (order n) [2..n-1]

units :: Integer -> [Integer]
units n = filter (\x -> gcd n x == 1) [1..n-1]

calcP :: Integer -> Int
calcP n = length $ filter (\x -> gcd n x == 1 && exp n x (n-1) == 1) [1..n-1]


isMthPrimRoot :: Integer -> Integer -> Integer -> Bool
isMthPrimRoot n m w = go w m
  where
    go acc pow = if pow == 1 then acc == 1 else acc /= 1 && go ((acc * w) `mod` n) (pow-1)

findMthPrimRoot :: Integer -> Integer -> Maybe Integer
findMthPrimRoot n m = find (\w -> trace ("Checking " ++ show w) $ isMthPrimRoot n m w) [0..n-1]

genPrime :: Int -> IO Integer
genPrime bits = do
    p <- randomRIO (2 ^ (bits - 1),2 ^ bits)
    --if isPrime p
    if isPrimeMR 40 p
      then pure p else genPrime bits

testFindPrimRoot :: IO ()
testFindPrimRoot = do
    let s = 128
    let genPrimes i = do
            p <- genPrime 20
            q <- genPrime 20
            let lam = lcm (p-1) (q-1)
            if lam `mod` s == 0
               -- && isPrime (lam `div` s)
                then pure (p,q,i) else genPrimes (i+1)
    (p,q,i) <- genPrimes 0
    putText $ "Iteration: " <> show i
    let n = p * q
    let lam = lcm (p-1) (q-1)
    let lamFactors = ordNub $ 1024 : primeFactors (lam `div` 1024)

    let isGen g =
            gcd g n == 1 &&
            all (\factor -> exp n g factor /= 1) lamFactors &&
            exp n g lam == 1

    let tryGen i = if i == 0 then pure Nothing else do
            let lroot :: Integer = fromMaybe (error "should work") $ find isGen [1..]
            let mroot = exp n lroot (lam `div` s)
            if isMthPrimRoot n 4 mroot then pure (Just mroot) else tryGen (i-1)

    tryGen 10 >>= \case
        Nothing -> testFindPrimRoot
        Just mroot -> putText $ show mroot

carm :: Integer -> Integer
carm x =
    let fs = primeFactors x in
    let fsn = ordNub fs in
    case fsn of
      [p] -> fromIntegral $
          if (length fs > 2) then eulerPhiFast x `div` 2 else eulerPhiFast x
      xs  -> foldr1 lcm $ map carm xs

type Nt = Z 323 -- 19 * 17
type Nst = Z 3515706497843 -- n ^ 5

legendreSymbol :: Integer -> Integer -> Integer
legendreSymbol p a = let res = exp p a ((p-1) `div` 2) in if res == p-1 then (-1) else res

-- -> y:snat{y < p * q} // /\ GMS.is_nonsqr (to_fe #p y) /\ GMS.is_nonsqr (to_fe #q y)}
-- -> r:snat{r < p * q} // /\ sqr r > 0 /\ sqr r *% y > 0}
genDataGM :: Int -> IO ()
genDataGM bits = do
    let genPrimes = do
            p <- genPrime (bits `div` 2)
            q <- genPrime (bits `div` 2)
            if p == q then genPrimes else pure (p,q)
    (p,q) <- genPrimes
    let n = p * q
    let genY = do
            y <- randomRIO (2^(bits-2), n-1)
            if legendreSymbol p y == -1 && legendreSymbol q y == -1
               then pure y
               else genY
    y <- genY
    let genR = do
            r <- randomRIO (2^(bits-2), n-1)
            if (r * r) `mod` n > 0 && (r * r * y) `mod` n > 0 then pure r else genR
    r <- genR
    print p
    print q
    print y
    print r

genDataPaillier :: Int -> IO ()
genDataPaillier bits = do
    let genPrimes = do
            p <- genPrime (bits `div` 2)
            q <- genPrime (bits `div` 2)
            if p == q then genPrimes else pure (p,q)
    (p,q) <- genPrimes
    let n = p * q
    let genR = do
            r <- genPrime (bits-1)
            if r < n && gcd r n == 1 then pure r else genR
    r <- genR
    let genM = do
            m <- genPrime (bits-1)
            if m < n then pure m else genM
    m <- genM
    putText (show p <> " " <> show q <> " " <> show r <> " " <> show m)

----------------------------------------------------------------------------
-- Inspecting FFT domain automorhisms
----------------------------------------------------------------------------

-- List of powers of w_n
type El n = [Z n]

type Pack n = [El n]

rotateRight :: [a] -> [a]
rotateRight xs = last xs : take (length xs - 1) xs

rotateRightTimes :: Int -> [a] -> [a]
rotateRightTimes 0 x = x
rotateRightTimes j x = rotateRightTimes (j-1) (rotateRight x)

opRotate :: Pack n -> Pack n
opRotate = rotateRight

opMulWj :: KnownNat n => Z n -> Pack n -> Pack n
opMulWj wj = map (map (<+> wj))

fftGen :: forall n. KnownNat n => Pack n
fftGen =
    let n' = natValI @n in
    map (\i -> map (\j -> Z i <*> Z j) [0..n'-1]) [0..n'-1]

allGroupComb :: forall n. KnownNat n => [Pack n -> Pack n]
allGroupComb =
    let n' = fromIntegral $ natValI @n in
    (rotateRightTimes <$> [0..n'-1]) ++
    (opMulWj <$> map (toZ . fromIntegral) [0..n'-1])

discoverAllElems :: forall n. KnownNat n => IO ()
discoverAllElems = do
    let n' = fromIntegral $ natValI @n
    let orig = fftGen @n
    let addMore (packs :: [Pack n]) depth =
          if depth > 10 then packs else
          let packs' = ordNub (packs ++ concatMap (\pack -> map (\a -> a pack) allGroupComb) packs) in
          if length packs' == length packs then packs else addMore packs' (depth+1)
    let saturated = addMore [orig] 0

    let origShifts = concatMap (\el -> map (\sh -> sh el) (rotateRightTimes <$> [0..n'-1])) orig
    let ofShifts (x::Pack n) = all (`elem` origShifts) x

    print (length saturated)
    forM_ saturated print
    putText "\nFiltered:"
    let filtered = filter ofShifts saturated
    print (length filtered)
    forM_ filtered print
