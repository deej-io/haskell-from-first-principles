module Filtering where

multiplesOf3 :: [Integer] -> [Integer]
multiplesOf3 = filter (\x -> (x `mod` 3) == 0)

howManyMultiplesOf3 :: [Integer] -> Int
howManyMultiplesOf3 = length . multiplesOf3

myFilter :: String -> [String]
myFilter = filter isExcluded . words
  where isExcluded = not . flip elem ["a", "an", "the"]
