import System.Environment as Env
import Control.Monad
import Text.Printf
import System.Random as Rnd
import Data.List as L
import System.IO (stdout)

nDoors :: Int
nDoors = 3

choose xs = (xs !! ) `fmap` Rnd.randomRIO (0, length xs - 1) 

-- keep the choice:
--decide choice _spoiler = choice

-- switch to another door:
decide choice spoiler = head . L.delete choice . L.delete spoiler $ [0..nDoors - 1]

runMontyHall :: Int -> Int -> IO Int
runMontyHall won 0 = return won
runMontyHall won total = do
  carDoor <- Rnd.randomRIO (0, nDoors - 1)
  choiceDoor <- Rnd.randomRIO (0, nDoors - 1)
  spoilerDoor <- choose (L.delete choiceDoor . L.delete carDoor $ [0..nDoors-1])
  -- hPrintf stdout "car=%d, choice=%d, spoiler=%d\n" carDoor choiceDoor spoilerDoor
  if carDoor == decide choiceDoor spoilerDoor
    then runMontyHall (succ won) (total - 1)
    else runMontyHall won (total - 1)

main = do
  args <- Env.getArgs
  exe <- Env.getProgName
  when (length args < 1) $ error (printf "Usage: %s <n>\n" exe)
  case reads (args !! 0) of
    [(n, _)] -> do
      won <- runMontyHall (0::Int) n
      let ratio = fromIntegral won / (fromIntegral n :: Double)
      hPrintf stdout "total=%d, won=%d, ratio=%f\n" n won ratio
    _ -> error "a number expected"
