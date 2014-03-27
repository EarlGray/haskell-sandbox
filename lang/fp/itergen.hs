{-
 - Playground for reading 
 - http://themonadreader.files.wordpress.com/2011/10/issue19.pdf
 -}
{-# LANGUAGE PackageImports, FlexibleContexts #-}

module IterGen where

import Control.Monad
import "mtl" Control.Monad.Trans

import Data.Functor.Identity (Identity(..))

{-
 - Trampoline
 -}
newtype Trampoline m r = Trampoline {
  bounce :: m (Either (Trampoline m r) r)
}

instance Monad m => Monad (Trampoline m) where
  return    = Trampoline . return . Right
  t >>= f   = Trampoline (bounce t >>= either (return . Left . (>>= f)) (bounce . f))

instance MonadTrans Trampoline where
  lift = Trampoline . liftM Right

{-
 - Generator
 -}
newtype Generator a m x = Generator {
  bounceGen :: m (Either (a, Generator a m x) x)
}

instance Monad m => Monad (Generator a m) where
  return    = Generator . return . Right
  t >>= f   = Generator (bounceGen t >>= either 
                                          (\(a, cont) -> return $ Left (a, cont >>= f))
                                          (bounceGen . f))

instance MonadTrans (Generator a) where
  lift = Generator . liftM Right

yield :: Monad m => a -> Generator a m ()
yield a = Generator (return $ Left (a, return ()))

runGenerator :: Monad m => Generator a m x -> m ([a], x)
runGenerator = run' id 
  where
    run' f g = bounceGen g >>= 
                either (\(a, cont) -> run' (f . (a:)) cont)
                       (\x -> return (f [], x))

-- example of a Generator, run with runGenerator:
gen1 = do
  lift $ putStr "Yielding one, "
  yield 1
  lift $ putStr "then two, "
  yield 2
  lift $ putStr "returning three: "
  return 3

gen2 = do
  replicateM 5 $ lift (readLn :: IO Int) >>= yield
  return ()
  

{-
 - Iteratee
 -}
newtype Iteratee a m x = Iteratee {
  bounceIter :: m (Either (a -> Iteratee a m x) x)
}

instance Monad m => Monad (Iteratee a m) where
  return    = Iteratee . return . Right
  t >>= f   = Iteratee (bounceIter t >>=
                        either (\cont -> return $ Left ((>>= f) . cont))
                               (bounceIter . f))

instance MonadTrans (Iteratee a) where
  lift = Iteratee . liftM Right

await :: Monad m => Iteratee a m a
await = Iteratee $ return $ Left return

runIteratee :: Monad m => [a] -> Iteratee a m x -> m x
runIteratee (a:rest) i =
  bounceIter i >>= either (\cont -> runIteratee rest (cont a)) return
runIteratee [] i =
  bounceIter i >>= either
      (\cont -> runIteratee [] (cont $ error "No more values to feed"))
      return

iter1 = do
  lift $ putStr "Enter two numbers: "
  a <- await
  b <- await
  lift $ putStrLn $ "their sum is " ++ show (a + b)


{-
 - Generalization: coroutine
 -}

newtype Coroutine s m r = Coroutine {
  resume :: m (Either (s (Coroutine s m r)) r)
}

instance (Functor s, Monad m) => Monad (Coroutine s m) where
  return    = Coroutine . return . Right
  t >>= f   = Coroutine (resume t >>= 
                          either (return . Left . fmap (>>= f))
                                 (resume . f))

instance Functor s => MonadTrans (Coroutine s) where
  lift = Coroutine . liftM Right

suspend :: (Monad m, Functor s) => s (Coroutine s m x) -> Coroutine s m x
suspend s = Coroutine (return (Left s))

type GTrampoline  m x = Coroutine Identity m x
type GGenerator a m x = Coroutine ((,) a)  m x
type GIteratee  a m x = Coroutine ((->) a) m x

gpause :: Monad m => GTrampoline m ()
gpause = suspend $ Identity $ return ()

gyield :: (Monad m, Functor ((,) x)) => x -> GGenerator x m ()
gyield x = suspend (x, return ())

gawait :: (Monad m, Functor ((->) x)) => GIteratee x m x
gawait = suspend return

runGTramp :: Monad m => GTrampoline m x -> m x
runGTramp t = resume t >>= either (runGTramp . runIdentity) return

runGGen :: Monad m => GGenerator x m r -> m ([x], r)
runGGen = run' id
  where
    run' f g = resume g >>= either
                              (\(x, cont) -> run' (f . (x:)) cont)
                              (\r -> return (f [], r))

runGIter :: Monad m => [a] -> GIteratee a m r -> m r
runGIter (x:rest) i =
  resume i >>= either (\cont -> runGIter rest (cont x)) return
runGIter [] i =
  resume i >>= either (\cont -> runGIter [] (cont $ error "No more values"))
                      return
