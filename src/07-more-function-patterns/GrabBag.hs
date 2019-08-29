module GrabBag where

-- All of the following are equivalent

mTh :: Num a => a -> a -> a -> a
mTh x y z = x * y * z

mTh' :: Num a => a -> a -> a -> a
mTh' x y = \z -> x * y * z

mTh'' :: Num a => a -> a -> a -> a
mTh'' x = \y -> \z -> x * y * z

mTh''' :: Num a => a -> a -> a -> a
mTh''' = \x ->  \y -> \z -> x * y * z

-- mth 3 :: Num a => a -> a -> a

addOneIfOdd :: Integral n => n -> n
addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = \x -> x + 1

-- GHCi says this is Integer -> Integer -> Integer. Why?
addFive = \x y -> (if x > y then y else x) + 5

mflip :: (a -> b -> c) -> b -> a -> c
mflip f x y = f y x
