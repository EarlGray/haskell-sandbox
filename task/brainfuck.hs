import Data.Char
import System.IO
import System.Environment (getArgs)

data Zipper a = Zipper [a] [a]  --

zyppmap f (Zipper r [])    = Zipper r [ f 0 ]
zyppmap f (Zipper r (x:l)) = Zipper r ((f x):l)

forward (Zipper l (x:r))         = Zipper (x:l) r
forwardWith val (Zipper l [])    = Zipper l [val]
forwardWith _   (Zipper l (x:r)) = Zipper (x:l) r

back (Zipper (x':l) (x:r))    = Zipper l (x':x:r)
backWith val (Zipper [] r)    = Zipper [val] r
backWith _   (Zipper (x:l) r) = Zipper l (x:r)

rewindFwd zp = go (forward zp) 0
  where go z@(Zipper l (x:r)) nest
          | x == '['  = go (Zipper (x:l) r) (nest + 1)
          | x == ']'  = if nest == 0 then z else go (Zipper (x:l) r) (nest - 1)
          | otherwise = go (Zipper (x:l) r) nest
rewindBack zp = go (back zp) 0
  where go z@(Zipper (x':l) (x:r)) nest
          | x == '['  = if nest == 0 then z else go (Zipper l (x':x:r)) (nest - 1)
          | x == ']'  = go (Zipper l (x':x:r)) (nest + 1)
          | otherwise = go (Zipper l (x':x:r)) nest

bfrun _ (Zipper _ []) = return ()
bfrun (Zipper tl []) prog = bfrun (Zipper tl [0]) prog
bfrun tape@(Zipper tl (v:tr)) prog@(Zipper pl (cmd:pr)) =
  let prog' = forward prog
  in case cmd of
      '+' -> bfrun (zyppmap succ tape)  prog'
      '-' -> bfrun (zyppmap pred tape)  prog'
      '<' -> bfrun (backWith 0 tape)    prog'
      '>' -> bfrun (forwardWith 0 tape) prog'
      '[' -> let prog' = if v == 0
                         then rewindFwd prog
                         else forward prog
             in bfrun tape prog'
      ']' -> let prog' = if v /= 0
                         then rewindBack prog
                         else forward prog
             in bfrun tape prog'
      '.' -> do
          putChar $ chr v
          hFlush stdout
          bfrun tape prog'
      ',' -> do
          c <- getChar
          bfrun (Zipper tl (ord c : tr)) prog'
      _   -> bfrun tape                prog'

main = do
    hSetBuffering stdin NoBuffering
    fname <- head `fmap` getArgs
    bfprog <- readFile fname
    bfrun (Zipper [] []) (Zipper [] bfprog)
