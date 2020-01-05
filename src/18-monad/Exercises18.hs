{-# LANGUAGE DeriveFunctor #-}
module Ch18.Exercises18 where

import Control.Monad (ap, join, liftM2)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg
  deriving (Eq, Functor, Show)

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) = const . const NopeDotJpg

instance Monad Nope where
  return _ = NopeDotJpg
  (>>=) = const . const NopeDotJpg

data BahEither b a =
    PLeft a
  | PRight b
  deriving (Eq, Functor, Show)

instance Applicative (BahEither b) where
  pure = return
  (<*>) = ap

instance Monad (BahEither b) where
  return = PLeft
  (PLeft a) >>= f = f a
  (PRight b) >>= _ = PRight b

newtype Identity a = Identity a
  deriving (Eq, Functor, Show)

instance Applicative Identity where
  pure = return
  (<*>) = ap

instance Monad Identity where
  return = Identity
  Identity a >>= f = f a

data List a = Nil | Cons a (List a)
  deriving (Eq, Functor, Show)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) = ap

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x Nil >>= f = f x
  Cons x xs >>= f = f x `append` (xs >>= f)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a  -> m b -> m c
l2 = liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:xs) f = liftM2 (++) ((:[]) <$> f x) (meh xs f)

-- tests

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
  arbitrary = oneof [PLeft <$> arbitrary, PRight <$> arbitrary]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(3, Cons <$> arbitrary <*> arbitrary), (1, return Nil)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

type Triple = (Int, String, Int)

main :: IO ()
main = do
  quickBatch (monad (undefined :: Nope Triple))
  quickBatch (monad (undefined :: BahEither Triple Triple))
  quickBatch (monad (undefined :: Identity Triple))
  quickBatch (monad (undefined :: List Triple))
