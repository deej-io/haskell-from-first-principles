module DatabaseProcessing where

import Data.Time
import Data.List

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

db :: [DatabaseItem]
db =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getTime []
  where getTime (DbDate d) b = d : b
        getTime _          b = b

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr getNumber []
  where getNumber (DbNumber d) b = d : b
        getNumber _            b = b

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr mostRecent' (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))
  where mostRecent' (DbDate d) b = max d b
        mostRecent' _          b = b

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sum' 0
  where sum' (DbNumber x) b = x + b
        sum' _            b = b

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length db)
