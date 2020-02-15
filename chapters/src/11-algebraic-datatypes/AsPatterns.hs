module AsPatterns where

import Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf sub@(s:ss) (x:xs)
  | s == x = isSubseqOf ss xs
  | otherwise = isSubseqOf sub xs

capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = toUpper c : cs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capitalizedPair . words
  where capitalizedPair word = (word, capitalizeWord word)

capitalizeParagraph :: String -> String
capitalizeParagraph para = unwords ((capitalizeWord . head $ paraWords) : (capitalizeRest paraWords))
  where paraWords = words para
        capitalizeRest (x:[]) = []
        capitalizeRest (x:y:xs)
          | last x == '.' = capitalizeWord y : capitalizeRest (y:xs)
          | otherwise = y : capitalizeRest (y:xs)
