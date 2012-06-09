module Ext2 where

import qualified Data.ByteString.Char8 as B
import Data.Binary
import Data.Binary.Get

type U32 = Word32
type U16 = Word16
type U8 = Word8
type S32 = Int32
type S16 = Int16
type S8 = Int8

newtype UUID = [Char8]

sbOffset = 1024
sbSize = 1024

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
    sMinorRevLevel      :: U16,
    sLastCheckTime      :: U32,
    sCheckInterval      :: U32,
    sCreatorOS          :: U32,
    sRevLevel           :: U32,
    sDefaultResUID      :: U16,
    sDefaultResGID      :: U16,
    
    sDynamic    :: Maybe SuperblockDyn
}

data SuperblockDyn = SuperblockDyn {
    sFirstIno           :: U32,
    sInodeSize          :: U16,
    sBlockGroupNr       :: U16,
    sFeatureSet         :: U32,
    sFeatureIncompat    :: U32,
    sFeatureROSet       :: U32,
    sUUID               :: UUID,
    sVolumeName         :: String,
    sLastMountedAt      :: String,
    sAlgoUsageBmap      :: U32
}


readSuperblock :: Handle -> Maybe IO Superblock
readSuperblock h = do
    hSeek h AbsoluteSeek sbOffset
    bytes <- B.hGet h sbSize
    return $ unpackSuperblock bytes 
