module Ch24.TryTry where

import Text.Trifecta
import Control.Applicative
import Data.Ratio

fraction :: Parser Rational
fraction = try $ do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

fractionOrDecimal :: Parser (Either Rational Integer)
fractionOrDecimal = (Left <$> fraction) <|> (Right <$> decimal)

intOrLetters :: Parser (Either Integer String)
intOrLetters = (Left <$> integer) <|> (Right <$> some letter)
