{-# LANGUAGE ForeignFunctionInterface #-}
{-# INCLUDE dlfcn.h #-}
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Data.ByteString as B
import Control.Monad (when)
import System.Exit
import Prelude as P

type DLHandle = ()
type VoidFunction = IO ()

rtldLazy = CInt 0x00001

foreign import ccall unsafe "dlopen"
	c_dlopen :: CString -> CInt -> IO (Ptr DLHandle)

foreign import ccall "dlerror"
  c_dlerror :: IO CString

foreign import ccall "dlsym"
  c_dlsym :: Ptr DLHandle -> CString -> IO (FunPtr VoidFunction)

foreign import ccall "dynamic"
  mkFun :: FunPtr VoidFunction -> VoidFunction

main = do
  h <- withCString "libso.so" $ flip c_dlopen rtldLazy
  when (nullPtr == h) $ do
    P.putStrLn "Failed to open library"
    c_dlerror >>= packCString >>= B.putStrLn
    exitWith $ ExitFailure 1

  func_ptr <- withCString "hello" $ c_dlsym h
  when (nullFunPtr == func_ptr) $ do
    P.putStrLn "Failed to get the symbol"
    exitWith $ ExitFailure 2

  let sayHello = mkFun func_ptr
  sayHello
