module Test.Vector.QuickCheck
where

import qualified Data.Vector as V
import Data.Vector ( Vector )

import Test.QuickCheck


instance (Arbitrary a) => Arbitrary (Vector a) where
  arbitrary = arbitrary >>= \(NonNegative (Small n)) ->
              V.replicateM n arbitrary

  shrink as
    | V.null as = []
    | l == 1    = shrinkElts
    | otherwise = [asLeft, asRight] ++ shrinkElts
    where
      l = V.length as
      (asLeft,asRight) = V.splitAt (l `div` 2) as
      shrinkElts = [ V.update as $ V.singleton (ix,e)
                   | ix <- [0..l-1]
                   , e <- shrink $ as V.! ix
                   ]
