{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module Ch21.Exercises where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- traverse :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f (t b)
newtype Identity a = Identity a
  deriving (Eq, Show, Functor, Foldable)

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)

newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Show, Functor, Foldable)

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

data Optional a = Nada | Yep a
  deriving (Eq, Show, Functor, Foldable)

instance Traversable Optional where
  traverse _ Nada    = pure Nada
  traverse f (Yep a) = fmap Yep (f a)

data List a = Nil | Cons a (List a)
  deriving (Eq, Show, Functor, Foldable)

instance Traversable List where
  traverse _ Nil         = pure Nil
  traverse f (Cons a as) = Cons <$> f a <*> traverse f as

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S n a) = S (fmap f n) (f a)

-- foldMap :: Monoid m => (a -> m) -> t a -> m
instance Foldable n => Foldable (S n) where
  foldMap f (S n a) = foldMap f n <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S n a) = S <$> traverse f n <*> f a

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty        = Empty
  fmap f (Leaf a    ) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap f Empty        = mempty
  foldMap f (Leaf a    ) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

  foldr _ a' Empty = a'
  foldr f a' (Leaf a) = f a a'
  foldr f a' (Node l a r) = f a (foldr f (foldr f a' l) r)

instance Traversable Tree where
  traverse f Empty        = pure Empty
  traverse f (Leaf a    ) = fmap Leaf (f a)
  traverse f (Node l a r) = Node <$> traverse f l <*> f a <*> traverse f r

-- tests

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = fmap Constant arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(5, fmap Yep arbitrary), (1, pure Nada)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(5, Cons <$> arbitrary <*> arbitrary), (1, pure Nil)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = frequency
    [ (3, Node <$> arbitrary <*> arbitrary <*> arbitrary)
    , (5, Leaf <$> arbitrary)
    , (1, pure Empty)
    ]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance (Functor n , Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) => EqProp (S n a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
  quickBatch
    (traversable (undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))
  quickBatch (traversable (undefined :: List (Int, Int, [Int])))
  quickBatch (traversable (undefined :: S Maybe (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Tree (Int, Int, [Int])))

