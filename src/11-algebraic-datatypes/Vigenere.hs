module Vigenere where

import Data.Char

shiftLetter :: Int -> Char -> Char
shiftLetter count char
  | isUpper char = chr $ ((ord char) + count) `mod` 26
  | isSpace char = char
  | otherwise = error $ char : " is not an upper case character or whitespace"

zipWithKeyword :: String -> String -> [(Int, Char)]
zipWithKeyword keyword message =
  let zip _ [] = []
      zip kw (' ':msg) = (0, ' ') : zip kw msg
      zip (k:kw) (m:msg) = (ord k - ord 'A', m) : zip kw msg
  in zip (cycle keyword) message

cipher :: String -> String -> String
cipher keyword message =
  map (uncurry shiftLetter) (zipWithKeyword keyword message)

uncipher :: String -> String -> String
uncipher keyword message =
  map (uncurry shiftLetter) . map negateFst $ (zipWithKeyword keyword message)
  where negateFst (i, c) = (-i, c)

main :: IO ()
main = do
  let result = cipher "ALLY" "MEET AT DAWN"
  putStrLn $ "cipher \"ALLY\" MEET AT DAWN = " ++ result
  putStrLn $ "uncipher \"ALLY\" " ++ result ++ " = " ++ uncipher "ALLY" result
