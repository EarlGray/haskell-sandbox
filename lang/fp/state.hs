module State (
  State(..),
  get, put, modify,
  evalState, execState
) where
import Control.Applicative

newtype State s a = State {
  runState :: s -> (a, s)
}

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put x = State (\_ -> ((), x))

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

unit :: a -> State s a
unit a = State (\s -> (a, s))

bind :: State s a -> (a -> State s b) -> State s b
bind st f = State (\s -> let (a, s') = runState st s in runState (f a) s')

instance Functor (State s) where
  fmap f t = State (\s -> let (a, s') = runState t s in (f a, s'))

instance Applicative (State s) where
  pure    = unit
  f <*> a = State (\s -> let (va, s') = runState a s in let (vf, s'') = runState f s' in (vf va, s''))

instance Monad (State s) where
  return  = unit
  (>>=)   = bind

evalState :: State s a -> s -> a
evalState t = fst . runState t

execState :: State s a -> s -> s
execState t = snd . runState t
