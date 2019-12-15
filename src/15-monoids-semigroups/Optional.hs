module Optional where

import           Data.Monoid
import           Data.Semigroup
import Test.QuickCheck

data Optional a = Nada | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) (Only a) (Only b) = Only $ a <> b
  (<>) a        Nada     = a
  (<>) Nada     b        = b

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

newtype First' a = First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' (Only a)) _ = First' $ Only a
  (<>) (First' Nada) (First' (Only a)) = First' $ Only a
  (<>) _ _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

-- tests

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary =
    frequency [(5, Only <$> arbitrary) ,(1, return Nada)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

test :: IO ()
test = do
  quickCheck (monoidAssoc :: Optional (Sum Int) -> Optional (Sum Int) -> Optional (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Optional (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: Optional (Sum Int) -> Bool)

  quickCheck (monoidAssoc :: First' (Sum Int) -> First' (Sum Int) -> First' (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: First' (Sum Int) -> Bool)
  quickCheck (monoidRightIdentity :: First' (Sum Int) -> Bool)
