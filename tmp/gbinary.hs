{-# LANGUAGE DeriveGeneric #-}
import Data.Binary
import Data.Binary.Get
import Data.Bits

import Data.Generics

data BMPHeader = BMPHeader {
    bmphMagic :: Word16,
    bmphFileSize :: Word32,
    bmphReserved :: Word32,
    bmphOffset :: Word32
} deriving (Generic, Show)

instance Binary BMPHeader

main = do
    print $ 
