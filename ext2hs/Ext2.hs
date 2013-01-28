module Ext2 where

import Data.Int
import Data.Word
import Data.Bits
import Data.List (intersperse)
import Data.Maybe (fromMaybe, fromJust)

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BI

import System.IO
import qualified System.IO.Strict as SIO
import Text.Printf
import Control.Applicative ((<$>), (<*), (<*>), liftA2)
import Control.Monad (join, replicateM, msum)


type U32 = Word32
type U16 = Word16
type U8 = Word8
type S32 = Int32
type S16 = Int16
type S8 = Int8

type Error = String

type BlockIndex = Word
type InodeIndex = Word

----- Shortcuts -----
a = undefined

int :: (Integral a, Num b) => a -> b
int = fromIntegral

------- Binary helpers -----
getByteList :: Int -> Get [Word8]
getByteList n = BS.unpack <$> getByteString n

dequote = init . drop 1     -- TODO: make it safe
showByteList = dequote . show . BS.pack . takeWhile (/= 0)

putByteList :: [Word8] -> Put
putByteList = putByteString . BS.pack

type Size = Word  -- may be Word64

class SizeOfAble a where
  sizeOf :: SizeOfAble a => a -> Size

  minSizeOf :: SizeOfAble a => a -> Size
  minSizeOf = sizeOf

data UnixTimestamp = UnixTime { unixSeconds :: U32 } deriving Show

data FileMode = FileMode { fmode :: U16 }

instance Show FileMode where
  show (FileMode mode) = t : rights
    where t = fromJust $ lookup (mode `shiftR` 12) $ zip [1, 2, 4, 6, 8, 0xa, 0xc] "pcdb-ls"
          rights = zipWith (\c f -> if f then c else '-') "rwxrwxwx" $ map (testBit mode) [8,7..0]

------- UUID ----

newtype UUID = UUID { uuidBytes :: [Word8] }

instance Binary UUID where
  get = UUID <$> getByteList 16
  put = putByteList . uuidBytes

instance Show UUID where
  show (UUID ws) = pretty $ splitLst ws $ reverse [4,2,2,2,6]
    where hexShow = concat . map (printf "%02x" :: Word8 -> String)
          splitLst xs = snd . foldr iter (xs,[])
          iter c (xs,ac) = (xs', ac ++ [chunk]) where (chunk,xs') = splitAt c xs
          pretty = concat . intersperse "-" . map hexShow

---- Constants -----
sbMagic :: Word16
sbMagic = 0xef53

defInoSize = 128
defFirstIno = 11

sbOffset = 1024
sbSize = 1024

---- Block devices

class BlockDevice bdev where
    readBlock :: bdev -> BlockIndex -> IO B.ByteString
    readBytes :: bdev -> (Integer,Int) -> IO B.ByteString
    blockSize :: bdev -> Word

data FileBlkDev = FileBlkDev {
    handle :: Handle,
    blksz :: Word
} deriving (Show)


instance BlockDevice FileBlkDev where
    blockSize = blksz

    readBlock bdev index =
        readBytes bdev ((toInteger index) * (toInteger blksz), blksz)
        where blksz = int $ blockSize bdev

    readBytes bdev (at,count) =
        hSeek h AbsoluteSeek at >> B.hGet h count
        where h = handle bdev


----  Compatible features
data E2FeatureCompat = E2FeatCompatAny
    | E2FeatCompat {
        e2FC_DirPrealloc,   e2FC_IMagicInodes,  e2FC_HasJournal :: Bool,
        e2FC_ExtAttrs,      e2FC_ResizeIno,     e2FC_DirIndex :: Bool
    }
  deriving (Show)

featuresCompat :: U32 -> E2FeatureCompat
featuresCompat 0xffffffff = E2FeatCompatAny
featuresCompat w = E2FeatCompat {
    e2FC_DirPrealloc =  testBit w 0,
    e2FC_IMagicInodes = testBit w 1,
    e2FC_HasJournal =   testBit w 2,
    e2FC_ExtAttrs =     testBit w 3,
    e2FC_ResizeIno =    testBit w 4,
    e2FC_DirIndex =     testBit w 5
}

instance Binary E2FeatureCompat where
  get = featuresCompat <$> getWord32le
  put = fail "TODO"

---- Readonly compatible features
data E2FeatureROCompat = E2FeatROCompatAny
    | E2FeatROCompat {  e2FRC_SparseSuper, e2FRC_LargeFile, e2FRC_BTreeDir :: Bool  }
  deriving (Show)

featuresROCompat :: U32 -> E2FeatureROCompat
featuresROCompat 0xffffffff = E2FeatROCompatAny
featuresROCompat w = E2FeatROCompat {
    e2FRC_SparseSuper = testBit w 0,
    e2FRC_LargeFile =   testBit w 1,
    e2FRC_BTreeDir =    testBit w 2
}

instance Binary E2FeatureROCompat where
  get = featuresROCompat <$> getWord32le
  put = fail "TODO"

---- Incompatible features
data E2FeatureIncompat = E2FeatIncompatAny
    | E2FeatIncompat {
        e2FIC_Compression, e2FIC_Filetype, e2FIC_Recover,
        e2FIC_JourDev, e2FIC_MetaBlockGr :: Bool    }
  deriving (Show)

featuresIncompat :: U32 -> E2FeatureIncompat
featuresIncompat 0xffffffff = E2FeatIncompatAny
featuresIncompat w = E2FeatIncompat {
    e2FIC_Compression = testBit w 0,
    e2FIC_Filetype =    testBit w 1,
    e2FIC_Recover =     testBit w 2,
    e2FIC_JourDev =     testBit w 3,
    e2FIC_MetaBlockGr = testBit w 4
}

instance Binary E2FeatureIncompat where
  get = featuresIncompat <$> getWord32le
  put = fail "TODO"

----- Creator OS
osEnum = zip [0,1..] ["Linux", "HURD", "Masix", "FreeBSD", "Lites"]

data OSEnum = OSEnum { osenum :: U32 }

instance Show OSEnum where
  show (OSEnum os) = fromMaybe (printf "Unknown (%d)" os) $ lookup os osEnum

----- Superblock

data Superblock = Superblock {
  sInodesCount        :: U32,
  sBlocksCount        :: U32,
  sReservedBlocksCount :: U32,
  sFreeBlocksCount    :: U32,
  sFreeInodesCount    :: U32,
  sFirstDataBlock     :: U32,
  sLogBlockSize       :: U32,
  sLogClusterSize     :: U32,
  sBlocksPerGroup     :: U32,
  sClustersPerGroup   :: U32,
  sInodesPerGroup     :: U32,
  sMountTime          :: U32,
  sWriteTime          :: U32,

  sMountsCount        :: U16,
  sMaxMountsCount     :: S16,
  sMagic              :: U16,
  sState              :: U16,
  sErrors             :: U16,

  sLastCheckTime      :: U32,
  sCheckInterval      :: U32,
  sCreatorOS          :: OSEnum,
  sRevLevel           :: (U32, U16),

  sDefaultResUID      :: U16,
  sDefaultResGID      :: U16,

  sDynRev     :: Maybe SbDynRev,
  sPrealloc   :: Maybe SbPrealloc,
  sJournaling :: Maybe SbJournaling
} deriving (Show)

bgetSuperblock = do
  uint13  <- replicateM 13   getWord32le
  ushort6 <- replicateM 6    getWord16le
  uint4   <- replicateM 4    getWord32le
  ushort2 <- replicateM 2    getWord16le
  return Superblock {
    sInodesCount = uint13 !! 0,           sBlocksCount = uint13 !! 1,
    sReservedBlocksCount = uint13 !! 2,   sFreeBlocksCount = uint13 !! 3,
    sFreeInodesCount = uint13 !! 4,       sFirstDataBlock = uint13 !! 5,
    sLogBlockSize = uint13 !! 6,          sLogClusterSize = uint13 !! 7,
    sBlocksPerGroup = uint13 !! 8,        sClustersPerGroup = uint13 !! 9,
    sInodesPerGroup = uint13 !! 10,       sMountTime = uint13 !! 11,
    sWriteTime = uint13 !! 12,

    sMountsCount = ushort6 !! 0,    sMaxMountsCount = int (ushort6 !! 1),
    sMagic = ushort6 !! 2,          sState = ushort6 !! 3,
    sErrors = ushort6 !! 4,
    sLastCheckTime = uint4 !! 0,    sCheckInterval = uint4 !! 1,
    sCreatorOS = OSEnum (uint4 !! 2),        sRevLevel = (uint4 !! 3, ushort6 !! 5),

    sDefaultResUID = ushort2 !! 0,  sDefaultResGID = ushort2 !! 1,

    sDynRev = Nothing,
    sPrealloc = Nothing,
    sJournaling = Nothing
  }

instance Binary Superblock where
  get = bgetSuperblock
  put = fail "TODO"

instance SizeOfAble Superblock where sizeOf _ = 1024


----- Superblock substructures

data SbDynRev = SbDynRev {
  sFirstIno           :: U32,
  sInodeSize          :: U16,
  sBlockGroupNr       :: U16,
  sFeatureCompat      :: E2FeatureCompat,
  sFeatureIncompat    :: E2FeatureIncompat,
  sFeatureROCompat    :: E2FeatureROCompat,
  sUUID               :: UUID,
  sVolumeName         :: String,
  sLastMountedAt      :: String,
  sAlgoUsageBmap      :: U32
} deriving (Show)

data SbPrealloc = SbPrealloc {
    sPreallocBlocks     :: U8,
    sPreallocDirBlocks  :: U8
} deriving (Show)

data SbJournaling = SbJournaling {
    sJournalUUID        :: UUID,
    sJournalInoNo       :: U32,
    sJournalDeviceNo    :: U32,
    sLastOrphan         :: U32,
    sHashSeed           :: [U32],       -- 4 U32
    sDefHashVersion     :: U8,
    sDefaultMountOpts   :: U32,
    sFirstMetaBlockGr   :: U32
} deriving (Show)

bgetSbDynRev = do
  fstIno <- getWord32le
  [inosz, blkgr] <- replicateM 2 getWord16le
  ftCompat <- get;   ftIncompat <- get;   ftROCompat <- get
  uuid <- get
  -- read and trim C strings:
  label  <- showByteList <$> getByteList 16
  lstmnt <- showByteList <$> getByteList 64
  algo <- getWord32le
  return SbDynRev {
    sFirstIno = fstIno,         sBlockGroupNr = blkgr,
    sInodeSize = inosz,
    sFeatureCompat = ftCompat,  sFeatureIncompat = ftIncompat,
    sFeatureROCompat = ftROCompat,
    sUUID = uuid,               sVolumeName = label,
    sLastMountedAt = lstmnt,    sAlgoUsageBmap = algo
  }

instance Binary SbDynRev where
  get = bgetSbDynRev
  put = fail "TODO"

bgetSbPrealloc = liftA2 SbPrealloc getWord8 getWord8 <* skip 2
instance Binary SbPrealloc where
  get = bgetSbPrealloc
  put = fail "TODO"

bgetSbJournaling = do
  juuid <- get
  [jno, jdevno, jlstorph] <- replicateM 3 getWord32le
  jhash <- replicateM 4 getWord32le
  defhashv <- getWord8
  defmntopts <- getWord32le;  fstmblkgr <- getWord32le;
  return SbJournaling {
    sJournalUUID = juuid,   sJournalInoNo = jno,    sJournalDeviceNo = jdevno,
    sLastOrphan = jlstorph, sHashSeed = jhash,      sDefHashVersion = defhashv,
    sDefaultMountOpts = defmntopts,                 sFirstMetaBlockGr = fstmblkgr
 }

instance Binary SbJournaling where
  get = bgetSbJournaling
  put = fail "TODO"

getSuperblock :: B.ByteString -> Either Superblock Error
getSuperblock bs = flip runGet bs $ do
  sb <- get
  let magic = int $ sMagic sb
  if magic /= sbMagic
  then return $ Right ("Wrong Ext2 magic : " ++ show magic)
  else if fst (sRevLevel sb) == 0
    then return $ Left sb
    else do
      sbdyn <- get
      let sb' = sb { sDynRev = Just sbdyn }
      if not $ e2FC_DirPrealloc (sFeatureCompat sbdyn)
      then return $ Left sb'
      else do
        sbpa <- get
        let sb'' = sb' { sPrealloc = Just sbpa }
        if not $ e2FC_HasJournal (sFeatureCompat sbdyn)
        then return $ Left sb''
        else do
          sbj <- get
          return $ Left (sb'' { sJournaling = Just sbj })

---- Blockgroups descriptors -----

data BlockGroupDescriptor = BlockGroupDescriptor {
  bgBlockBitmap :: BlockIndex,
  bgInodeBitmap :: BlockIndex,
  bgInodeTable :: BlockIndex,
  bgFreeBlocksCount :: U16,
  bgFreeInodesCount :: U16,
  bgUsedDirsCount :: U16
} deriving (Show)

bgetBlkGrDescr = do
  bitmaps <- replicateM 3  getWord32le
  counts <- replicateM 3 getWord16le
  skip 14
  return BlockGroupDescriptor {
    bgBlockBitmap = int (bitmaps!!0),
    bgInodeBitmap = int (bitmaps!!1),
    bgInodeTable = int (bitmaps!!2),

    bgFreeBlocksCount = counts!!0,
    bgFreeInodesCount = counts!!1,
    bgUsedDirsCount = counts!!2
  }

instance SizeOfAble BlockGroupDescriptor where sizeOf _ = 32

instance Binary BlockGroupDescriptor where
  get = bgetBlkGrDescr
  put = fail "TODO"

---- Inode structure -----

directBlocksCount = 12

data Inode = Inode {
  iMode :: FileMode,
  iUID :: U16,
  iSize :: U32,
  iAccessTime :: UnixTimestamp,
  iCreatTime :: UnixTimestamp,
  iModifTime :: UnixTimestamp,
  iDeletTime :: UnixTimestamp,
  iGID :: U16,
  iLinksCount :: U16,
  iBlocksCount :: U32,
  iFlags :: U32,
  iHeadBlocks :: [BlockIndex],
  iIndirectBlock :: BlockIndex,
  iDoubleIndirBlock :: BlockIndex,
  iTrippleIndirBlock :: BlockIndex
} deriving (Show)

instance Binary Inode where
  get = bgetInode
  put = fail "TODO"

bgetInode = do
  mode <- FileMode <$> getWord16le
  uid <- getWord16le
  size <- getWord32le
  atime <- UnixTime <$> getWord32le
  ctime <- UnixTime <$> getWord32le
  mtime <- UnixTime <$> getWord32le
  dtime <- UnixTime <$> getWord32le
  gid <- getWord16le
  nlinks <- getWord16le
  nblocks <- getWord32le
  flags <- getWord32le
  getWord32le -- osd1
  hdblocks <- replicateM directBlocksCount getWord32le
  ind1b <- getWord32le
  ind2b <- getWord32le
  ind3b <- getWord32le
  return Inode {
    iMode = mode, iUID = uid, iSize = size,
    iAccessTime = atime, iCreatTime = ctime, iModifTime = mtime, iDeletTime = dtime,
    iGID = gid,  iLinksCount = nlinks,  iBlocksCount = nblocks, iFlags = flags,
    iHeadBlocks = map int hdblocks,
    iIndirectBlock = int ind1b,
    iDoubleIndirBlock = int ind2b,
    iTrippleIndirBlock = int ind3b }

---- Directory entity structure ----
data DirEntry = DirEntry {
  deInode :: InodeIndex,
  deRecSize :: U16,
  deFileType :: U8,
  deName :: String
} deriving (Show)

instance Binary DirEntry where
  get = bgetDirEntry
  put = fail "TODO"

instance SizeOfAble DirEntry where
  minSizeOf _ = 8
  sizeOf de = int $ deRecSize de

bgetDirEntry = do
  ino <- getWord32le
  recsz <- getWord16le
  namelen <- getWord8
  ftype <- getWord8
  namestr <- getByteList $ int (int recsz - minSizeOf (a::DirEntry))
  return DirEntry {
    deInode = int ino,
    deRecSize = recsz,
    deFileType = ftype,
    deName = showByteList $ take (int namelen) namestr
  }

getDirEntries :: Get [DirEntry]
getDirEntries = do
  bytesRemained <- int <$> remaining
  if bytesRemained < minSizeOf (a::DirEntry)
  then return []
  else do
    de <- bgetDirEntry
    if deInode de /= 0
    then (de:) <$> getDirEntries
    else return []


---- Ext2FS structure -----

data Ext2FS = Ext2FS {
  super :: Superblock,
  bgds :: [BlockGroupDescriptor],
  bdev :: FileBlkDev
}

instance BlockDevice Ext2FS where
  readBytes fs = readBytes (bdev fs)
  readBlock fs = readBlock (bdev fs)
  blockSize = fsblockSize

fsblockSize :: Ext2FS -> Word
fsblockSize fs = 1024 `shiftL` (int $ sLogBlockSize $ super fs)

fsInoSize :: Ext2FS -> U16
fsInoSize fs = fromMaybe defInoSize (sInodeSize <$> (sDynRev $ super fs))

getBlockGroupTable :: Int -> B.ByteString -> [BlockGroupDescriptor]
getBlockGroupTable nr = runGet $ replicateM nr get

-----
readSuperblock :: BlockDevice b => b -> IO (Either Superblock Error)
readSuperblock bdev = getSuperblock <$> readBytes bdev (sbOffset, sbSize)

readBlockGroupTable :: BlockDevice b => b -> Superblock -> IO [BlockGroupDescriptor]
readBlockGroupTable bdev sb = getBlockGroupTable nblkgr <$> readBlock bdev blkno
  where (n', rest') = (sBlocksCount sb) `divMod` (sBlocksPerGroup sb)
        nblkgr = int $ n' + if rest' == 0 then 0 else 1
        blkno = int $ (sFirstDataBlock sb) + 1

readInode :: Ext2FS -> InodeIndex -> IO Inode
readInode fs ino = decode <$> readBytes (bdev fs) (inoOffset, int inosz)
  where (bgNo, bgInd) = (ino - 1) `divMod` (int $ sInodesPerGroup $ super fs)
        inosz = int $ fsInoSize fs
        blksz = int $ fsblockSize fs
        inoTableStart = bgInodeTable $ bgds fs !! int bgNo
        fstBlock = int $ sFirstDataBlock (super fs)
        inoOffset = toInteger $ blksz * (inoTableStart) + inosz * int bgInd

readBlockAsBlockList :: Ext2FS -> BlockIndex -> IO [BlockIndex]
readBlockAsBlockList fs b = runGet bget <$> readBlock (bdev fs) b
  where bget = do
          remained <- remaining
          replicateM ((int remained) `div` 4) (int <$> getWord32le)

readBlockList :: Ext2FS -> Inode -> IO [BlockIndex]
readBlockList fs ino = do
  tripples <- readTrippleIndirects fs (iIndirectBlock ino)
  doubles <- readDoubleIndirects fs (iDoubleIndirBlock ino)
  singles <- readIndirects fs (iTrippleIndirBlock ino)
  return $ takeWhile (/= 0) $ iHeadBlocks ino ++ singles ++ doubles ++ tripples
  where
    readIndirects fs 0 =    return []
    readIndirects fs blk =  readBlockAsBlockList fs blk

    readDoubleIndirects fs 0 = return []
    readDoubleIndirects fs blk =
      concat <$> (join $ mapM (readIndirects fs) . takeWhile (/=0) <$> readIndirects fs blk)

    readTrippleIndirects fs 0 = return []
    readTrippleIndirects fs blk =
      concat <$> (join $ mapM (readDoubleIndirects fs) <$> readIndirects fs blk)

readBlocks :: Ext2FS -> [BlockIndex] -> IO B.ByteString
readBlocks fs [] = return B.empty
readBlocks fs (b:bs) = B.append <$> readBlock (bdev fs) b <*> readBlocks fs bs


readDirEntries :: Ext2FS -> Inode -> IO [DirEntry]
readDirEntries fs ino = do
  blocks <- readBlockList fs ino
  contents <- readBlocks fs blocks
  return $ runGet getDirEntries contents

openExt2Image :: FilePath -> IO (Either Ext2FS Error)
openExt2Image fname = do
  h <- openBinaryFile fname ReadMode
  let bd = FileBlkDev h 1024
  sbbs <- readBytes bd (sbOffset,sbSize)
  case getSuperblock sbbs of
    Right err -> return $ Right err
    Left sb -> do
      bgdt <- readBlockGroupTable bd sb
      return $ Left $ Ext2FS sb bgdt bd

---- debug
lstype ft = fromMaybe '?' $ lookup ft $ zip [0,1..] "?-dcbpsl"
lsdir fs ino = readDirEntries fs ino >>= mapM_ (\(DirEntry ino _ ft name) -> printf "%d\t%c\t%s\n" ino (lstype ft) name)
