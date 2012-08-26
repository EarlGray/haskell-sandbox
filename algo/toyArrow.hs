{-# LANGUAGE Arrows #-}

import Control.Arrow (returnA)

idA :: a -> a
idA = proc a -> returnA -< a

plusOne :: Int -> Int
plusOne = proc a -> returnA -< (a+1)
