import System.IO
import Network.Socket
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan

type Msg = (Int, String)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    chan <- newChan
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 20
    forkIO $ serverThread chan
    mainLoop sock chan 0

serverThread :: Chan Msg -> IO ()
serverThread chan = do
    (_, msg) <- readChan chan
    serverThread chan

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO $ handleConn conn chan nr
    mainLoop sock chan (nr + 1)

handleConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
handleConn (sock, addr) chan nr = do
    let broadcast msg = writeChan chan (nr, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    putStrLn $ "Connection from " ++ show addr
    hPutStrLn hdl "Hello! Please introduce yourself:\n"
    user <- liftM init $ hGetLine hdl 
    broadcast $ "--> " ++ user ++ " entered\n"
    hPutStrLn hdl $ "Welcome, " ++ user ++ "\n"
    chan' <- dupChan chan
    listener <- forkIO $ fix $ \loop -> do
            (nr', line) <- readChan chan'
            when (nr /= nr') $ hPutStrLn hdl line
            loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
            line <- liftM init (hGetLine hdl)
            case line of
                "quit" -> hPutStrLn hdl "Bye!"
                _ -> do
                    broadcast $ user ++ ": " ++ line
                    loop
    killThread listener
    broadcast $ "<-- " ++ user ++ " quit.\n"
    hClose hdl
    
