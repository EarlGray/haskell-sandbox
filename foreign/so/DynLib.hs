{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Exit
import System.Environment
import Control.Monad (when)
import Prelude as P

type DLHandle = ()
type VoidFunction = IO ()

exitWithMsg :: String -> Int -> IO a
exitWithMsg msg err = do
  P.putStrLn msg
  c_dlerror >>= peekCString >>= P.putStrLn
  exitWith $ ExitFailure err

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
  args <- getArgs
  let funcname = if P.null args then "hello" else P.head args

  h <- withCString "libso.so" $ flip c_dlopen rtldLazy
  when (nullPtr == h) $ exitWithMsg "Failed to open library" 1

  func_ptr <- withCString funcname $ c_dlsym h
  when (nullFunPtr == func_ptr) $ exitWithMsg "Failed to get the symbol" 2

  let sayHello = mkFun func_ptr
  sayHello
