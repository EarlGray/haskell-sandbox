{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative
import Control.Monad (unless)

import Data.IORef
import Control.Monad.State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

{-
 -  See: 
http://tel.github.io/2014/07/12/mutable_algorithms_in_immutable_languges_part_1/
 -}

import Mem


{-
 - Mem IO instance
 -}

instance Mem IO where
    newtype Ref IO = IORef' { unwrapIORef' :: IORef Int } deriving Eq
    type    Val IO = Int

    ref     = fmap IORef' . newIORef
    deref   = readIORef . unwrapIORef'
    set r v = writeIORef (unwrapIORef' r) v

newtype UfIO v a = UfIO { runUfIO :: IO a }
        deriving (Functor, Applicative, Monad)

instance Mem (UfIO v) where
    newtype Ref (UfIO v) = 
        UfIORef { getUfIORef :: IORef (Node_ (UfIO v) v) } deriving (Eq)
    type Val (UfIO v) = Node_ (UfIO v) v

    ref a   = UfIO (UfIORef <$> newIORef a)
    deref r = UfIO (readIORef $ getUfIORef r)
    set r v = UfIO (writeIORef (getUfIORef r) v)


{-
 - a pure Mem instance
 -}

data UfIMap v = UfIMap {
    count :: Int,
    mem :: IntMap (Node_ (UfIntMap v) v)
}

uf0 :: UfIMap v
uf0 = UfIMap { count = 0, mem = IntMap.empty }

newtype UfIntMap v a = 
  UfIntMap { unUfIntMap :: State (UfIMap v) a }
    deriving (Functor, Applicative, Monad)

runUfIntMap :: UfIntMap v a -> a
runUfIntMap = flip evalState uf0 . unUfIntMap
  where
    
instance Mem (UfIntMap v) where
    newtype Ref (UfIntMap v) = UfIntMapRef { getId :: Int } deriving Eq
    type    Val (UfIntMap v) = Node_ (UfIntMap v) v
    
    set r v = UfIntMap $ do
                modify (\s -> s { mem = IntMap.insert (getId r) v (mem s) })
    ref v  = UfIntMap $ do
                c <- gets count
                modify (\s -> s { count = c + 1, mem = IntMap.insert c v (mem s) })
                return $ UfIntMapRef c
    deref r = UfIntMap $ do
                Just v <- gets (IntMap.lookup (getId r) . mem)
                return v

type UF r a = (Mem r, Val r ~ Node_ r a)

data Node_ r a =
  Node_ {
    parent :: Maybe (Ref r),
    rank   :: Int,
    value  :: a
  }
    
newtype Node r = Node (Ref r)

node :: UF r a => a -> r (Node r)
node a = do
    r <- ref (Node_ { parent = Nothing, rank = 0, value = a })
    return $ Node r

link :: UF r a => Node r -> Node r -> r()
link n1 n2 = do
    Node p1 <- find n1
    Node p2 <- find n2
    unless (p1 == p2) $ adopt p1 p2
  where
    adopt x y = do
      nx <- deref x 
      ny <- deref y
      case compare (rank nx) (rank ny) of
        EQ -> do set x (nx { rank = rank nx + 1 })
                 set y (ny { parent = Just x })
        LT -> set x (nx { parent = Just y })
        GT -> set y (ny { parent = Just x })

find :: UF r a => Node r -> r (Node r)
find (Node r) = do
    Node p <- findRec (Node r)
    unless (r == p) $ alter (\n -> n { parent = Just p }) r
    return $ Node p
  where
    findRec :: UF r a => Node r -> r (Node r)
    findRec (Node r) = do
        n <- deref r
        case parent n of
          Nothing -> return (Node r)
          Just p  -> find (Node p)

connected :: UF r a => Node r -> Node r -> r Bool
connected n1 n2 = do
    Node p1 <- find n1
    Node p2 <- find n2
    return (p1 == p2)  -- representative nodes are the same

{-
 - test
 -}
exPure = runUfIntMap computation
  where
    computation = do
        n1 <- node 1
        n2 <- node 2
        link n1 n2
        connected n1 n2

bug :: Bool
bug = let n1 = runUfIntMap $ do { node () ; node () ; node () }
          conn = runUfIntMap $ do
                    n2 <- node ()
                    connected n1 n2
      in conn

bug2 :: Bool
bug2 = let n1 = runUfIntMap $ node ()
           conn = runUfIntMap $ do
             n2 <- node ()
             connected n1 n2
       in conn

main = do
    print exPure
    print bug2

