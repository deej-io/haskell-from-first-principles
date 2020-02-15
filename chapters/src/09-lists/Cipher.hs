module Cipher where

import Data.Char

shiftLetter :: Int -> Char -> Char
shiftLetter count char
  | isUpper char = shift' 'A'
  | isLower char = shift' 'a'
  | otherwise = error $ char : " is not an alphabetic character"
    where shift' base =
            chr $ (((ord char) - (ord base) + count) `mod` 26) + (ord base)

ceaser :: Int -> String -> String
ceaser count = map . shiftLetter $ count

unCeaser :: Int -> String -> String
unCeaser count = map . shiftLetter . negate $ count
