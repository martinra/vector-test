{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}
module Test.Vector.SmallCheck
where

import qualified Data.Vector as V
import Data.Vector ( Vector )
import Numeric.Natural ( Natural )

import Test.Natural
import Test.SmallCheck.Series


instance (Monad m, Serial m a) => Serial m (Vector a) where
  series = series >>= \n ->
    V.sequence $ V.iterateN (fromIntegral (n :: Natural)) decDepth $
    decDepth series
