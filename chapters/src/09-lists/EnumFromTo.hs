module EnumFromTo where

eft :: (Ord a, Eq a, Enum a) => a -> a -> [a]
eft from to
  | from > to = []
  | otherwise = go from to []
    where go f t acc
            | f == t = [t]
            | otherwise = f : go (succ f) t acc

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft
