{-# LANGUAGE DeriveFunctor #-}

module Exercises17 where

import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Functor, Show)
data Two a b = Two a b deriving (Eq, Functor, Show)
data Three a b c = Three a b c deriving (Eq, Functor, Show)
data Three' a b = Three' a b b deriving (Eq, Functor, Show)
data Four a b c d = Four a b c d deriving (Eq, Functor, Show)
data Four' a b = Four' a a a b deriving (Eq, Functor, Show)

instance Applicative Pair where
  pure a = Pair a a
  (Pair f1 f2) <*> (Pair a1 a2) = Pair (f1 a1) (f2 a2)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (Two a1 f) <*> (Two a2 b) = Two (a1 <> a2) (f b)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (Three a1 b1 f) <*> (Three a2 b2 c) = Three (a1 <> a2) (b1 <> b2) (f c)

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  (Three' a1 f1 f2) <*> (Three' a2 b1 b2) = Three' (a1 <> a2) (f1 b1) (f2 b2)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (Four a1 b1 c1 f) <*> (Four a2 b2 c2 d) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (f d)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (Four' a1 a2 a3 f) <*> (Four' a1' a2' a3' b) = Four' (a1 <> a1') (a2 <> a2') (a3 <> a3') (f b)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

-- tests

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (applicative (Pair ("", "", "") ("", "", "")))
  quickBatch (applicative (Two ("", "", "") ("", "", "")))
  quickBatch (applicative (Three ("", "", "") ("", "", "") ("", "", "")))
  quickBatch (applicative (Three' ("", "", "") ("", "", "") ("", "", "")))
  quickBatch (applicative (Four ("", "", "") ("", "", "") ("", "", "") ("", "", "")))
  quickBatch (applicative (Four' ("", "", "") ("", "", "") ("", "", "") ("", "", "")))
