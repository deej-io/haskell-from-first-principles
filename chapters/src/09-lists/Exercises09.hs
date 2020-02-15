module Exercises09 where

import Data.Char

allUppers :: String -> String
allUppers = filter isUpper

capitalise :: String -> String
capitalise "" = ""
capitalise (c:cs) = toUpper c : cs

shout :: String -> String
shout "" = ""
shout (c:cs) = toUpper c : shout cs

firstCapitalisedLetter :: String -> Char
firstCapitalisedLetter = toUpper . head
