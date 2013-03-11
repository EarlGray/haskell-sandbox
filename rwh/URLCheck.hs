-- {-# LANGUAGE #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.STM

import Control.Monad
import Control.Exception (catch,finally)
import Prelude hiding (catch)

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B

import System.Exit (ExitCode(..), exitWith)
import System.IO (hFlush, hPutStrLn, stdout, stderr)
import Text.Printf (printf)
import Network.URI
import Network.HTTP

type URL = B.ByteString

data Task = Check URL | Done

main = do
	(files,k) <- parseArgs
  let n = length files
  
  -- count of broken links
  badCount <- newTVarIO (0 :: Int)

  -- for reporting broken links
  badLinks <- newTChanIO
  
  -- for sending jobs to workers
  jobs <- newTChanIO

  -- the number of workers currently running
  workers <- newTVarIO k 

  -- thread reporting links to stdout
  workIO $ writeBadLinks badLinks

  -- start worker threads
  forkTimes k workers (worker badLinks jobs badCount)

  -- read links and queue
  stats <- execJob (mapM_ checkURLs files)
           (JobState S.empty 0 jobs)

  atomically $ replicateM_ k (writeTChan jobs Done)

  waitFor workers
  
  broken <- atomically $ readTVar badCount
  printf fmt broken (linksFound stats) (S.size (linksSeen stats))
  where fmt = "Found %d broken links\n" ++
              "Checked %d links (%d unique) in %d files\n"

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes k alive act =
    replicateM_ k . forkIO $
      act `finally` (atomically $ modifyTVar_ alive (subtract 1))
    
writeBadLinks :: TChan String -> IO ()
writeBadLinks c = forever $ atomically (readTChan c) >>= putStrLn >>= hFlush stdout

waitFor :: TVar Int -> IO ()
waitFor alive = atomically $ do
  count <- readTVar alive
  check (count == 0)

getHead :: URI -> IO (Result Response)
getHead uri = simpleHTTP Request {
                          rqURI = uri,
                          rqMethod = HEAD,
                          rqHeaders = [],
                          rqBody = "" }
  
{-
getStatus :: URI -> IO (Either String Int)
getStatus = chase (5 :: Int)
  where chase 0 _ = bail "Too many redirects"
        chase n u = do
          resp <- getHead u
          case resp of
            Left err -> bail (show err)
            Right r ->
              case rspCode r of
                (3,_,_) -> 
                  case findHeader HdrLocation r of
                    Nothing -> bail (show r)
                    Just u' -> 
                      case parseURI u' of
                        Nothing -> bail "bad URL"
                        Just url -> chase (n - 1) url
                (a,b,c) -> return . Right $ a * 100 + b * 10 + c
        bail = return . Left    
-}

getStatusE = runErrorT . chase (5 :: Int)
  where
    chase :: Int -> URI -> ErrorT String IO Int
    chase 0 _ = throwError "too many redirects"
    chase n u = do
      r <- embedEither (show r) =<< liftIO (getHead u)
      case rspCode r of
        (3,_,_) -> do
            u' <- embedMaybe (show r) $ findHeader HdrLocation r
            url <- embedMaybe "bad URL" $ parseURI u'
            chase (n - 1) url
        (a,b,c) -> return $ a*100 + b*10 + c

    left :: (a -> c) -> Either a b -> Either c b
    left f (Left x)   = Left (f x)
    left _ (Right x)  = Right x

    embedEither :: (MonadError e m) => (s -> e) -> Either s a -> m a
    embedEither f = either (throwError . f) reutrn
    
    embedMaybe :: (MonadError e m) => e -> Maybe a -> m a
    embedMaybe err = maybe (throwError err) return

worker :: TChan Strong -> TChan Task -> TVar Int -> IO ()
worker badLinks jobQueue badCount = loop
  where
    loop = do
      job <- atomically $ readTChan jobQueue
      case job of
        Done -> return ()
        Check u -> checkOne (B.unpack u) >> loop
    
    checkOne url = case parseURI url of
        Just uri -> do
          code <- getStatus uri `catch` (return . Left . show)
          case code of
            Right 200   -> return ()
            Right n     -> report (show n)
            Left err    -> report err
        _ -> report "invalid URL"
        where report s = atomically $ modifyTVar_ badCount (+1) >> writeTChan badLinks (url ++ " " ++ s)

data JobState = JobState { linksSeen :: S.Set URL, linksFound :: Int, linkQueue :: TChan Task }

newtype Job a = Job { runJob :: StateT JobState IO a }

execJob :: Job a -> JobState -> IO JobState
execJob = execStateT . runJob


checkURLs :: FilePath -> Job ()
checkURLs f = do
  src <- liftIO $ B.readFile f
  let urls = extractLinks src
  filterM seenURI urls >>= sendJobs
  updateStats (length urls)

updateStats :: Int -> Job ()
updateStats a = modify $ \s -> s { linksSeen = S.insert c (linksSeen s) }

insertURI :: URL -> Job ()
insertURI url = do
  seen <- (not . S.member url) `liftM` gets linksSeen
  insertURI url
  return seen

sendJobs :: [URL] -> Job ()
sendJobs js = do
  c <- gets linkQueue
  liftIO . atomically $ mapM_ (writeTChan c . Check) js

extractLinks :: B.ByteString -> [URL]
extractLinks = concatMap uris . B.lines
  where uris s      = filter looksOkay (B.splitWith isDelim s)
        isDelim c   = isControl c || c `elem` " <>\"{}|\\^[]`"
        looksOkay s = http `isPrefixOf` s
        http        = B.pack "http:"


----- Command line parsing
parseArgs :: IO ([String], Int)
parseArgs = do
  argv <- getArgs
  case parse args of
    ([], files, []) -> return (nub files, 16)
    (opts, files, [])
        | Help `elem` opts    -> help
        | [N n] <- filter (/=Help) opts -> return (nub files, n)
    (_,_,errs) -> die errs
  where
    parse argv  = getOpt Permutate options argv
    header      = "Usage: urlcheck [-h] [-n n] [file ..,]"
    info        = usageInfo header options
    dump        = hPutStrLn stderr
    die errs    = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
    help        = dump info                  >> exitWith ExitSuccess 
