{-# LANGUAGE QuasiQuotes #-}

module LogParser where

import Text.Trifecta
import Data.Functor
import Control.Applicative
import Data.Char
import Text.RawString.QQ

data Time = Time Integer Integer deriving (Eq, Show)
data Entry = Entry Time String deriving (Eq, Show)
data Date = Date Integer Integer Integer deriving (Eq, Show)
data Day = Day Date [Entry] deriving (Eq, Show)

comment :: Parser ()
comment = (try (someSpace >> string "--") <|> string "--") >> skipLine

endOfLine :: Parser ()
endOfLine = try comment <|> newline $> ()

skipLine :: Parser ()
skipLine = skipMany (noneOf "\n") >> skipOptional (char '\n') $> ()

parseTime :: Parser Time
parseTime = Time <$> (integer <* char ':') <*> integer

parseEntry :: Parser Entry
parseEntry = Entry <$> parseTime <*> manyTill anyChar endOfLine

parseDate :: Parser Date
parseDate = Date <$> year <*> month <*> day
  where year = string "# " *> integer <* char '-'
        month = integer <* char '-'
        day = integer

parseDay :: Parser Day
parseDay = do
  skipMany (noneOf "#")
  date <- parseDate
  skipOptional (comment >> skipLine)
  entries <- some parseEntry
  return $ Day date entries

parseLog :: Parser [Day]
parseLog = some (try parseDay) <* eof

main = print $ parseString parseLog mempty logStr

logStr :: String
logStr = [r|
-- wheee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast? 
09:00 Bumped head, passed out
13:36 Wake up, headache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep
|]
