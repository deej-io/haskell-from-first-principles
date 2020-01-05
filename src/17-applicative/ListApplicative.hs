{-# LANGUAGE DeriveFunctor #-}

module ListApplicative where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data List a = Nil | Cons a (List a)
  deriving (Functor, Eq, Show)

append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Applicative List where
  pure a = Cons a Nil
  Nil         <*> _   = Nil
  _           <*> Nil = Nil
  (Cons f fs) <*> xs  = fmap f xs `append` (fs <*> xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (2, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

main :: IO ()
main = quickBatch (applicative (Nil :: List (String, String, String)))
