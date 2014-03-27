{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Control.Monad

foreign import ccall "math.h sin"
  c_sin :: CDouble -> CDouble

fsin :: Double -> Double
fsin x = realToFrac (c_sin (realToFrac x))

foreign import ccall "stdlib.h rand"
  c_rand :: IO CInt

rand :: Int -> IO Int
rand r = liftM ((`mod` r) . fromIntegral) $ c_rand

main = do
  print $ fsin 1.57
  r <- rand 6
  print r
