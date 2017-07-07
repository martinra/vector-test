{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  #-}
module Test.Vector.SmallCheck
where

import Control.Monad ( guard )
import qualified Data.Vector as V
import Data.Vector ( Vector )

import Test.SmallCheck.Series


instance (Monad m, Serial m a) => Serial m (Vector a) where
  series = do
    n <- series
    guard $ n >=0 
    V.sequence $ V.iterateN n decDepth $ decDepth series
