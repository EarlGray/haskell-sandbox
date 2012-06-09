
todoList   :: [ IO () ]
todoList   = [ putChar 'a' ,
               do putChar 'b'
                  putChar 'c' ,
               do c <- getChar
                  putChar c ] 

_seq_       :: [IO ()] -> IO ()
_seq_       = foldr (>>) (return ())
                
_putStr s   = _seq_ (map putChar s)

main :: IO()
main = do _seq_ todoList
          putChar '\n'
