module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
  | n >= 0 && n < 10 = getWord
  | otherwise = "unknown"
    where getWord =
            ["zero", "one", "two", "three", "four",
             "five", "six", "seven", "eight", "nine"] !! n

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = digits d ++ [m]
    where (d, m) = divMod n 10

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
