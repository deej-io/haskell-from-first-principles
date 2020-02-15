module Ch22.ReadingComprehension where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

--
-- fmap :: (a -> b) -> (r -> a) -> (r -> b)
--        
--  r --> |r -> a| --> |a -> b| --> b
--
-- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
--
--  r --> |r -> a| --> |           |
--    |                |r -> a -> b| --> b
--    `--------------> |           | 
--
-- (r -> a -> b -> c) <$> (r -> a) <*> (r -> b)
--
--  r --> |r -> a| -->|                |
--    |-> |r -> b| -->|r -> a -> b -> c| --> c
--    `-------------->|                |
--

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  -- fmap f (Reader ra) = Reader $ \r -> f (ra r)
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure = Reader . const
  Reader rab <*> Reader ra = Reader $ rab <*> ra
  -- Reader rab <*> Reader ra = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  Reader ra >>= arb = Reader $ \r -> runReader (arb (ra r)) r
