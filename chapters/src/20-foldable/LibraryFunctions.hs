module Ch20.LibraryFunctions where

import           Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny . foldMap (Any . (== a))

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs
  | null xs = Nothing
  | otherwise = Just $ foldr min (head $ toList xs) xs

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs
  | null xs = Nothing
  | otherwise = Just $ foldr max (head $ toList xs) xs

null' :: (Foldable t) => t a -> Bool
null' = getAny . foldMap (Any . const True)

length :: (Foldable t) => t a -> Int
length = foldr (\_ a -> a + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' m  = foldr (\x acc -> m x <> acc) mempty


