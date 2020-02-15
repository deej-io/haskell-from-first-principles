module StandardFunctions where

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny pred = myOr . map pred

myElem :: Eq a => a -> [a] -> Bool
myElem x = myAny (==x)

myReverse :: [a] -> [a]
myReverse xs = rev xs []
  where
    rev [] a = a
    rev (x:xs) a = rev xs (x:a)

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go x xs
  where
    go acc [] = acc
    go acc (x:xs)
      | f x acc == GT = go x xs
      | otherwise = go acc xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = myMaximumBy . flip
