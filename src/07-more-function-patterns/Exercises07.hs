module Exercises07 where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = d
  where (xLast, _) = x `divMod` 10
        (_    , d) = xLast `divMod` 10

hunsDigit :: Integral a => a -> a
hunsDigit = tensDigit . (`div` 10)

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  False -> x
  True  -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b         = y
  | otherwise = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

-- module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip = read . show

main = do
  print ((roundTrip 4) :: Integer)
  print (id 4)
