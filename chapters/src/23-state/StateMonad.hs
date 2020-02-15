{-# LANGUAGE TupleSections #-}

module Ch23.StateMonad where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> let (a, s') = g s in (f a, s')

instance Applicative (Moi s) where
  pure a = Moi (a, )
  Moi f <*> Moi g = Moi $ \s ->
    let (f', s' ) = f s
        (a , s'') = g s'
    in  (f' a, s'')

instance Monad (Moi s) where
  return = pure
  Moi f >>= g = Moi $ \s -> 
    let (a, s') = f s
     in runMoi (g a) s'
