module SortingCommon (
    module Data.Array.Base,
    module Data.Array.Unboxed,
    module Data.Array.ST,
    module Control.Monad,
    module Control.Monad.ST,
    swap
) where

import Control.Monad
import Control.Monad.ST

import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.ST
import Data.Array.Unboxed

swap :: STUArray s Int Int -> Int -> Int -> ST s ()
swap a i j = do
  t <- unsafeRead a i
  unsafeRead a j >>= unsafeWrite a i
  unsafeWrite a j t

