module Exercises10 where

import Data.Foldable (foldl')

stops = "pbtdkg"
vowels = "aeiou"

ex1a stops vowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

ex1b stops vowels = [('p', y, z) | y <- vowels, z <- stops]

ex1b' stops vowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

ex1c = ex1a

-- finds the mean length of the words in the sentence
seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

ex3 :: String -> Double
ex3 x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) =
  if x == False
  then False
  else myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . fmap f

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = foldr (\x' a -> if x' == x then True else a) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (== x)

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x a -> f x : a) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x a -> if f x then x : a else a) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp (x:xs) = foldl' f x xs
  where f x a = if cmp x a == GT then x else a

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp (x:xs) = foldl' f x xs
  where f x a = if cmp x a == LT then x else a
