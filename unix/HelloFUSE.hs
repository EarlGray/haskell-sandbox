module Main where

{-
 -  Install HFuse package from cabal and compile the file with -threaded
 -
 -}

import Control.Exception
import Data.Maybe
import Data.List
import System.IO
import System.Fuse
import System.Posix
import System.Posix.Types
import System.Posix.Time
import Numeric (showOct)

helloFUSE :: FuseOperations Handle
helloFUSE = defaultFuseOps {
    fuseInit = hfuseInit,
    fuseDestroy = hfuseDestroy,
    fuseGetFileSystemStats = hfuseVFSStat,

    fuseAccess = hfuseAccess,
    fuseGetFileStat = hfuseGetAttr,

    fuseOpenDirectory = hfuseOpenDir,
    fuseReadDirectory = hfuseReadDir
}

fsstat :: FileSystemStats
fsstat = FileSystemStats {
      fsStatBlockSize = 512,
      fsStatBlockCount = 1,
      fsStatBlocksFree = 0,
      fsStatBlocksAvailable = 0,
      fsStatFileCount = 20,
      fsStatFilesFree = 0,
      fsStatMaxNameLength = 255
}

fileattrs = [
    --(".",       FileStat Directory   (CMode 0o755) 1 (CUid 0) (CGid 0) (CDev 0) 512 1 0 0 0),
    ("/",       FileStat Directory   (CMode 0o755) 1 (CUid 0) (CGid 0) (CDev 0) 512 1 0 0 0),
    ("/hello",  FileStat RegularFile (CMode 0o644) 1 (CUid 0) (CGid 0) (CDev 0) 512 1  0 0 0)]


hfuseInit = putStrLn "hfuseInit" >> hFlush stdout

hfuseDestroy = putStrLn "hfuseDestroy" >> hFlush stdout

hfuseVFSStat :: String -> IO (Either Errno FileSystemStats)
hfuseVFSStat _ = return $ Right fsstat

hfuseAccess :: FilePath -> Int -> IO Errno
hfuseAccess fname perm = do
    putStrLn $ "### ACCESS(" ++ fname ++ ", " ++ showOct perm ")"
    case lookup fname fileattrs of
      Just _ -> return eOK
      Nothing -> return eNOENT

hfuseGetAttr :: FilePath -> IO (Either Errno FileStat)
hfuseGetAttr fpath = do
    putStrLn $ "### GETATTR(" ++ fpath ++ ")"
    case lookup fpath fileattrs of
      Just fstat -> return $ Right fstat
      Nothing -> return $ Left eNOENT

hfuseOpenDir :: FilePath -> IO Errno
hfuseOpenDir dirpath = do
    putStrLn $ "### OPENDIR(" ++ dirpath ++ ")"
    case dirpath of
      "/" -> return eOK
      _ -> return eNOENT

hfuseReadDir :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
hfuseReadDir dirpath = do
    putStrLn $ "### READDIR (" ++ dirpath ++ ")"
    uid <- getRealUserID
    gid <- getRealGroupID
    time <- epochTime
    let mkFName (fname,info) | fname == dirpath = Just (".",info)
                             | otherwise = (flip (,) info) `fmap` stripPrefix dirpath fname
        fattrs = mapMaybe mkFName fileattrs
    case dirpath of
      "/" -> return $ Right fattrs
      _ -> return $ Left eNOENT

handleExeption :: SomeException -> IO Errno
handleExeption e = do
    putStrLn "ERROR: FUSE exception"
    print e
    hFlush stdout
    return eFAULT

main :: IO ()
main = fuseMain helloFUSE handleExeption
