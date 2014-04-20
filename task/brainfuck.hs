import Data.Char
import System.IO
import System.Environment (getArgs)

data Zypper a = Zypper [a] [a]  --

zyppmap f (Zypper r [])    = Zypper r [ f 0 ]
zyppmap f (Zypper r (x:l)) = Zypper r ((f x):l)

current (Zypper _ [])    = 0
current (Zypper _ (v:_)) = v

setCurrent val (Zypper l []) = Zypper l [val]
setCurrent val (Zypper l (_:r)) = Zypper l (val:r)

forward (Zypper l (x:r))         = Zypper (x:l) r
forwardWith val (Zypper l [])    = Zypper l [val]
forwardWith _   (Zypper l (x:r)) = Zypper (x:l) r

back (Zypper (x':l) (x:r))    = Zypper l (x':x:r)
backWith val (Zypper [] r)    = Zypper [val] r
backWith _   (Zypper (x:l) r) = Zypper l (x:r)

rewindFwd zp = go (forward zp) 0
  where go z@(Zypper l (x:r)) nest
          | x == '['  = go (Zypper (x:l) r) (nest + 1)
          | x == ']'  = if nest == 0 then z else go (Zypper (x:l) r) (nest - 1)
          | otherwise = go (Zypper (x:l) r) nest
rewindBack zp = go (back zp) 0
  where go z@(Zypper (x':l) (x:r)) nest
          | x == '['  = if nest == 0 then z else go (Zypper l (x':x:r)) (nest - 1)
          | x == ']'  = go (Zypper l (x':x:r)) (nest + 1)
          | otherwise = go (Zypper l (x':x:r)) nest

bfrun _ (Zypper _ []) = return ()
bfrun tape prog@(Zypper pl (cmd:pr)) =
  let prog' = forward prog
  in case cmd of
      '+' -> bfrun (zyppmap succ tape)  prog'
      '-' -> bfrun (zyppmap pred tape)  prog'
      '<' -> bfrun (backWith 0 tape)    prog'
      '>' -> bfrun (forwardWith 0 tape) prog'
      '[' -> let prog' = if current tape == 0
                         then rewindFwd prog
                         else forward prog
             in bfrun tape prog'
      ']' -> let prog' = if current tape /= 0
                         then rewindBack prog
                         else forward prog
             in bfrun tape prog'
      '.' -> do
          putChar $ chr $ current tape
          hFlush stdout
          bfrun tape prog'
      ',' -> do
          c <- getChar
          bfrun (setCurrent (ord c) tape) prog'
      _   -> bfrun tape                prog'

main = do
    hSetBuffering stdin NoBuffering
    fname <- head `fmap` getArgs
    bfprog <- readFile fname
    bfrun (Zypper [] []) (Zypper [] bfprog)
