newtype State s a = State {
  runState :: s -> (a, s)
}

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put x = State (\_ -> ((), x))

instance Functor (State s) where
  fmap f t = State (\s -> let (a, s') = runState t s in (f a, s'))

instance Monad (State s) where
  return x = State (\s -> (x, s))
  x >>= f  = State (\s -> let (a, s') = runState x s in runState (f a) s')

evalState :: State s a -> s -> a
evalState t s = fst $ runState t s
