module DaPhone where

import Data.Char
import Data.List

type Label = Char
type Digit = Char
type Presses = Int

data Key = Key
  { label :: Label
  , digits :: [Digit]
  } deriving (Show, Eq)

type Keyboard = [Key]

data DaPhone = DaPhone { keyboard :: Keyboard } deriving Show

daPhone = DaPhone
  [ Key '1' "1"
  , Key '2' "abc2"
  , Key '3' "def3"
  , Key '4' "ghi4"
  , Key '5' "jkl5"
  , Key '6' "mno6"
  , Key '7' "pqrs7"
  , Key '8' "tuv8"
  , Key '9' "wxyz9"
  , Key '0' " 0"
  , Key '*' "^*"
  , Key '#' ".,#"
  ]

keyForDigit :: DaPhone -> Digit -> Key
keyForDigit phone digit = findIn (keyboard phone)
  where findIn (key:keys)
          | digit `elem` (digits key) = key
          | otherwise = findIn keys

pressesForDigit :: [Digit] -> Digit -> Int
pressesForDigit (d:ds) digit
  | digit == d = 1
  | otherwise = succ $ pressesForDigit ds digit

keyPressesForDigit :: DaPhone -> Digit -> (Label, Presses)
keyPressesForDigit phone digit = (label key, pressesForDigit (digits key) digit)
  where key = keyForDigit phone digit

reverseTaps :: DaPhone -> Digit -> [(Label, Presses)]
reverseTaps phone c
  | c == ' ' = [('0', 1)]
  | isUpper c = ('*', 1) : reverseTaps phone (toLower c)
  | otherwise = [keyPressesForDigit phone c]

cellPhonesDead :: DaPhone -> String -> [(Label, Presses)]
cellPhonesDead phone = concatMap $ reverseTaps phone

fingerTaps :: [(Label, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

mostPopularElement :: (Ord a) => [a] -> a
mostPopularElement = head . head . sortBy (\x y -> (length y) `compare` (length x)) . group . sort

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopularElement

mostPopularWord :: String -> String
mostPopularWord = mostPopularElement . words

coolestLtr :: [String] -> Char
coolestLtr = mostPopularElement . filter (/=' ') . concat

coolestWord :: [String] -> String
coolestWord = mostPopularWord . intercalate " "
