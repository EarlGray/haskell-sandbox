{-# LANGUAGE ScopedTypeVariables, RankNTypes, FlexibleContexts #-}
module SortingCommon (
    module Data.Array.Base,
    module Data.Array.Unboxed,
    module Data.Array.ST,
    module Control.Monad,
    module Control.Monad.ST,
    swap, sortUArray
) where

import Control.Monad
import Control.Monad.ST

import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.ST
import Data.Array.Unboxed

sortUArray :: forall i e s. (Ix i, IArray UArray e, MArray (STUArray s) e (ST s)) => 
              (STUArray s i e -> ST s ()) -> UArray i e -> ST s (UArray i e)
sortUArray sort ua = do
    mua <- thaw ua -- :: ST s1 (STUArray s1 i e)
    sort mua
    freeze mua

swap :: STUArray s Int Int -> Int -> Int -> ST s ()
swap a i j = do
  t <- unsafeRead a i
  unsafeRead a j >>= unsafeWrite a i
  unsafeWrite a j t

