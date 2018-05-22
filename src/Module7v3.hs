{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

-- | Basic vector spaces.

module Module7v3 () where

import Universum

import qualified Data.List as L

import Lib.Field
import Lib.Vector

----------------------------------------------------------------------------
-- 7.5
----------------------------------------------------------------------------

express :: Field f => [[f]] -> [f] -> [f]
express base x = map L.last $ unMatrix $ gaussSolve $ mtranspose $ Matrix $ base ++ [x]
    {-
    traceShow (take 15 es) $
    traceShow (take 800 allCombs) $
    fromMaybe (error "kek...") $
    find (\c -> let s = foldl1 vplus (map (uncurry scal) $ c `zip` base)
                in --traceShow s $
                   s == x) allCombs
  where
    es :: [Double]
    es = 0 : concat [[e,-e] | e <- [1..] ]
    allCombs :: [[Double]]
    allCombs = L.nub $ allCombsGo 1
      where
        allCombsGo m = replicateM n (take m es) ++ allCombsGo (m+1)
    n = length base
    -}

e75 :: IO ()
e75 = do
    let base :: [[Rational]]
        base = [[1,3,2], [2,-1,3], [1,0,2]]
    let base2 :: [[Rational]]
        base2 = [[-1,0,2], [3,1,-1], [1,0,1]]
    print $ map (express base) base2

    let v :: Vect Rational
        v = Vect [2,3,1]
    let w = Vect [-1,4,-2]
    print $ (vlen v, vlen w, dot v w, angle v w)

{-

λ> e75
[[(-4) % 3,(-4) % 1,25 % 3],[8 % 3,7 % 1,(-41) % 3],[1 % 3,1 % 1,(-4) % 3]]
(3.7416573867739413,4.58257569495584,8 % 1,1.0853880929848332)

-}
