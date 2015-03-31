module Test.Vector.QuickCheck
where

import qualified Data.Vector as V
import Data.Vector ( Vector )

import Test.QuickCheck


instance (Arbitrary a) => Arbitrary (Vector a) where
  arbitrary = arbitrary >>= \(NonNegative (Small n)) ->
              V.replicateM n arbitrary

  shrink as | V.null as = []
            | otherwise =    [ as ]
                          ++ [ V.head as `V.cons` as'
                             | as' <- shrink (V.tail as) ]
                          ++ [ a' `V.cons` as
                             | a' <- shrink (V.head as) ]
