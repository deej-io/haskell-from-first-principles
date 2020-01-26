{-# LANGUAGE TupleSections #-}
module Ch23.Exercises where

newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (State s) where
  pure a = State (a, )
  State f <*> State g = State $ \s ->
    let (f', s' ) = f s
        (a , s'') = g s'
    in  (f' a, s'')

instance Monad (State s) where
  return = pure
  State f >>= g = State $ \s -> 
    let (a, s') = f s
     in runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

exec :: State s a -> s -> s
exec (State sa) s = snd (sa s)

eval :: State s a -> s -> a
eval (State sa) s = fst (sa s)

modify :: (s -> s) -> State s ()
modify f = get >>= put . f
