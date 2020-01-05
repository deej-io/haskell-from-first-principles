module ZipListApplicative where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype ZipList' a = ZipList' [a]
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
   where
    xs' = let (ZipList' l) = xs in take 100 l
    ys' = let (ZipList' l) = ys in take 100 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure = ZipList' . repeat
  (ZipList' fs) <*> (ZipList' xs) = ZipList' $ zipWith ($) fs xs

main :: IO ()
main = quickBatch (applicative (ZipList' [("", "", "")]))
