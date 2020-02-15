module Validation where

import Test.QuickCheck.Arbitrary
import Test.QuickCheck (oneof)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (Success f ) <*> x            = fmap f x
  (Failure e1) <*> (Failure e2) = Failure (e1 <> e2)
  (Failure e ) <*> _            = Failure e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [Success <$> arbitrary, Failure <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = quickBatch (applicative (undefined :: Validation (String, String, String) (Int, Int, Int)))
