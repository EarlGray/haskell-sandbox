-- see: http://en.wikibooks.org/wiki/Haskell/Monad_transformers
import Control.Monad
import Control.Monad.Trans

import Data.Char

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
    return = MaybeT . return . Just
    x >>= f = MaybeT $ do mbVal <- runMaybeT x
                          case mbVal of
                            Nothing -> return Nothing
                            Just val -> runMaybeT $ f val

instance Monad m => MonadPlus (MaybeT m) where
    mzero     = MaybeT $ return Nothing
    mplus x y = MaybeT $ do mbVal <- runMaybeT x
                            case mbVal of
                              Nothing  -> runMaybeT y
                              Just _ -> return mbVal

instance MonadTrans MaybeT where
    lift = MaybeT . (liftM Just)


newtype StateT s m a = StateT { runStateT :: (s -> m (a, s)) }

instance (Monad m) => Monad (StateT s m) where
    return x = StateT (\s -> return (x, s))
    (StateT x) >>= f =
        StateT $ \s -> do
                   (v, s') <- x s
                   runStateT (f v) s'

instance (MonadPlus m) => MonadPlus (StateT s m) where
    mzero = StateT $ \s -> mzero
    (StateT x1) `mplus` (StateT x2) = StateT $ \s -> (x1 s) `mplus` (x2 s)

instance MonadTrans (StateT s) where
    lift c = StateT $ \s -> do
                              val <- c
                              return (val, s)

isValid :: String -> Bool
isValid s = length s >= 8 && any isAlpha s && any isNumber s && any isPunctuation s

getValidPassphrase :: MaybeT IO String
getValidPassphrase = do
    s <- lift getLine
    guard (isValid s)       -- MonadPlus
    return s

askPassphrase :: MaybeT IO ()
askPassphrase = do
    lift $ putStrLn "Insert you new passphrase:"
    value <- getValidPassphrase
    lift $ putStrLn "Storing it..."
