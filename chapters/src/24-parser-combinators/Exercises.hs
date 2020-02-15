module Ch24.Exercises where

import Text.Trifecta
import Data.Char (digitToInt)
import Control.Applicative ((<|>))

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

nonDigit :: Parser Char
nonDigit = noneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = toInteger . digitsToInt . fmap digitToInt <$> many parseDigit
  where digitsToInt = foldl joinDigits 0
        joinDigits acc x = acc * 10 + x

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = PhoneNumber <$> (skipOptional (char '(') *> digits 3) <*> digits 3 <*> digits 4
  where digits n = read <$> count n (parseDigit <* skipMany nonDigit)
