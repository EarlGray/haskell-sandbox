{-# LANGUAGE DeriveDataTypeable #-}

import Data.Bits

import Data.Binary
import Data.Binary.Get
import Data.Binary.Generic
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Data

data BMPHeader = BMPHeader {
    bmphMagic :: Word16,
    bmphFileSize :: Word32,
    bmphReserved :: Word32,
    bmphOffset :: Word32
} deriving (Data, Typeable, Show)

main = do
    bytes <- BL.readFile "tru256.bmp"
    print $ runGet (getGeneric :: Get BMPHeader) bytes
