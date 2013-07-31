import Data.Bits
import Data.Int
import Data.Maybe
import Data.List (unfoldr)
import Control.Monad (when, forM_)
import System.Environment (getArgs)

import Data.Vector (Vector, (!?), (//), (!))
import qualified Data.Vector as V

type TactCount = Integer
type Reg = Int
data RegVal = RVal { rval :: Int }

regCount = 8
bitness = 30

type Program = Vector Command
data Command 
    = Load Reg RegVal
    | Add Reg Reg

data CPU = CPU {
    eflags      :: RegVal,
    progcounter :: RegVal,
    reg         :: V.Vector RegVal,
    tacts       :: TactCount,
    cmds        :: Program
}

binary :: (Integral a, Num a, Bits a) => a -> String
binary = reverse . take bitness . unfoldr (\n -> Just (showZeroOne $ n`mod`2, n`div`2))
    where showZeroOne 0 = '0'
          showZeroOne 1 = '1'

readCmd line = 
    case head ws of
        "load" -> Load (regname (ws!!1)) (RVal (read (ws!!2) :: Int))
        "add" -> Add (regname (ws!!1)) (regname (ws!!2))
        _ -> error $ "Парсер: невідома команда у рядку " ++ line
  where ws = words line
        regname w = case head w of
                      'r' -> (read $ tail w) :: Int
                      _ -> error "Парсер: Не регістр"

showCmd (Load r (RVal val)) = "load r" ++ show r ++ " " ++ show val
showCmd (Add r1 r2) = "add r" ++ show r1 ++ " r" ++ show r2

--instance Read Command where read = readCmd
instance Show Command where show = showCmd

parse :: FilePath -> IO Program
parse filename = do
    contents <- readFile filename
    return . V.fromList . map readCmd . lines $ contents

dump :: CPU -> IO ()
dump cpu = do
    let pc = rval $ progcounter cpu
    putStrLn $ "====== Такт: " ++ show(tacts cpu) ++ " ================"
    putStrLn $ " IR: " ++ show (cmds cpu ! pc)
    forM_ (zip [0,1..] (V.toList $ reg cpu)) $ \(i, v) ->
        putStrLn $ " R" ++ show i ++ ": " ++ binary (rval v)
    putStrLn $ " PS: " ++ binary (rval $ eflags cpu)
    putStrLn $ " PC: " ++ show pc

step :: CPU -> IO CPU 
step cpu = do
    let pc = rval $ progcounter cpu

    let mbCmd = cmds cpu !? pc
    when (isNothing mbCmd) $ 
        error $ "Програмна адреса " ++ show pc ++ " не містить команди"
    let cmd = fromJust mbCmd

    let (upd_reg, eflags' ) = case cmd of
                                 Load r val -> ([(r, val)], eflags cpu)
                                 Add r1 r2 -> let v1 = rval $ reg cpu ! r1
                                                  v2 = rval $ reg cpu ! r2
                                              in ([(r1, RVal (v1 + v2))], eflags cpu)

    let cpu' = cpu {
        eflags = eflags',
        reg = reg cpu // upd_reg
    }

    dump cpu'
    getChar
    
    step cpu' {
        progcounter = RVal ((rval $ progcounter cpu') + 1),
        tacts = tacts cpu' + 1
    }

main = do
    args <- getArgs
    when (null args) $ error "Не вистачає імені файла для інтерпретації"

    program <- parse (head args)
    
    let startCPU = CPU {
        eflags = RVal 0,
        progcounter = RVal 0,
        reg = V.replicate regCount (RVal 0),
        tacts = 0,
        cmds = program
    }
    step startCPU
