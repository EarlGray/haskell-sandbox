{-
 -  A simple utility for comparing two BMP files
 -  It computes  percentage of  RGB pixel components 
 -  that differ by more than the specified threshold
 -}

import Data.Binary.Get as BG
import Data.Binary.Put as BP

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL

import Data.Word
import Data.Int
import Data.Char
import Data.Bits (xor)
import Data.Maybe

import System.IO
import Control.Monad (when)
import Control.Applicative ((<$>))
import System.Environment (getArgs)

int :: (Integral a, Num b) => a -> b
int = fromIntegral  -- to keep code less cluttered

data BitmapFileHeader = BmpFileHeader {
    bfhFileSize :: Word32,
    bfhImageOffset :: Word32
} deriving (Show, Eq)

data BitmapInfoHeader = BmpInfoHeader {
    bihSize :: Word32,
    bihWidth, bihHeight :: Int32,
    bihBitsPerPixel :: Word16,
    bihCompression :: Word32,
    bihImageSize :: Word32,
    bihHorResolution, bihVertResolution :: Word32
} deriving (Show, Eq)

data BitmapData = BmpData {
    bdBitsPerPixel :: Word16,
    bdData :: B.ByteString
} deriving (Show)

binGetBFH :: Get BitmapFileHeader
binGetBFH = do
    magic <- getByteString 2
    when (magic /= BI.packChars "BM") $ error "BMP magic is invalid"
    bfhFileSize <- getWord32le
    skip 4
    bfhImageOffset <- getWord32le
    return $ BmpFileHeader bfhFileSize bfhImageOffset

binPutBFH :: BitmapFileHeader -> BP.Put
binPutBFH bfh = do
    putByteString $ BI.packChars "BM"
    putWord32le $ bfhFileSize bfh
    putWord32le 0
    putWord32le $ bfhImageOffset bfh

binGetBIH :: Get BitmapInfoHeader
binGetBIH = do
    bihWidth <- int <$> getWord32le
    bihHeight <- int <$> getWord32le
    nColorPlanes <- getWord16le
    when (nColorPlanes /= 1) $ error "Number of color planes must be 1"
    bitsPerPixel <- getWord16le
    compression <- getWord32le
    imageSize <- getWord32le
    horizResolution <- getWord32le
    vertResolution <- getWord32le
    skip 8
    return $ BmpInfoHeader 40 bihWidth bihHeight bitsPerPixel compression imageSize horizResolution vertResolution

binPutBIH bih = do
    mapM_ putWord32le [ bihSize bih, int $ bihWidth bih, int $ bihHeight bih ]
    mapM_ putWord16le [ 1, bihBitsPerPixel bih ]
    mapM_ putWord32le [ 0, bihImageSize bih, bihHorResolution bih, bihVertResolution bih ]
    putWord64le 0

bmpReadBMFileHeader :: Handle -> IO BitmapFileHeader
bmpReadBMFileHeader h = do
    hSeek h AbsoluteSeek 0
    bfhBytes <- BL.hGet h 14
    return $ BG.runGet binGetBFH bfhBytes

bmpReadBMInfoHeader :: Handle -> IO BitmapInfoHeader
bmpReadBMInfoHeader h = do
    bihSize <- (int . BG.runGet getWord32le) `fmap` BL.hGet h 4
    --when (bihSize /= 40) $ do
    --    error $ "Unknown format: not a BitmapInfoHeader, size of DIB is " ++ show bihSize
    
    bihBytes <- BL.hGet h $ int (bihSize - 4)
    let bih = runGet binGetBIH bihBytes
    return bih { bihSize = bihSize } 

bmpReadData :: BitmapFileHeader -> BitmapInfoHeader -> Handle -> IO B.ByteString
bmpReadData bfh bih h = do
    hSeek h AbsoluteSeek $ int (bfhImageOffset bfh)
    B.hGet h (int (bihImageSize bih))

bmpWriteBMFileHeader :: BitmapFileHeader -> Handle -> IO ()
bmpWriteBMFileHeader bfh h = do
    hSeek h AbsoluteSeek 0
    BL.hPut h $ BP.runPut (binPutBFH bfh)

bmpWriteBMInfoHeader :: BitmapInfoHeader -> Handle -> IO ()
bmpWriteBMInfoHeader bih h = 
    BL.hPut h $ BP.runPut (binPutBIH bih)

writeBmpFile :: FilePath -> BitmapFileHeader -> BitmapInfoHeader -> B.ByteString -> IO ()
writeBmpFile fname bfh bih bitmap = withFile fname WriteMode $ \h -> do
    bmpWriteBMFileHeader bfh h
    bmpWriteBMInfoHeader bih h
    hSeek h AbsoluteSeek (int $ bfhImageOffset bfh)
    B.hPut h bitmap

main = do
    args <- getArgs
    when (length args < 2) $ error "Usage: <program> <file>.bmp"
    
    let fname1 = args !! 0
    let fname2 = args !! 1
    withFile fname1 ReadMode $ \h1 ->
      withFile fname2 ReadMode $ \h2 -> do
        bfh1 <- bmpReadBMFileHeader h1
        bfh2 <- bmpReadBMFileHeader h2
        when (bfh1 /= bfh2) $ error "Different file formats"

        bih1 <- bmpReadBMInfoHeader h1
        bih2 <- bmpReadBMInfoHeader h2
        when (bih1 /= bih2) $ error "Different image formats"

        data1 <- bmpReadData bfh1 bih1 h1
        data2 <- bmpReadData bfh2 bih2 h2

        let xored = BI.packBytes $ B.zipWith xor data1 data2
        writeBmpFile "diff.bmp" bfh1 bih1 xored

        let difffunc = \b1 b2 -> let xored = b1 `xor` b2
                                 in if xored < 0x40 then Nothing else Just xored
        let diffcount = length $ catMaybes $ zipWith difffunc (BI.unpackBytes data1) (BI.unpackBytes data2)
        putStrLn $ "Percent of pixels has changed: " 
        print $ diffcount * 100 `div` B.length data1
