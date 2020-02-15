module Exercises05 where

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (_, y) = y

i :: a -> a
i x = x;

c :: a -> b -> a
c x _ = x

c'' :: b -> a -> b
c'' = c

c' :: a -> b -> b
c' _ x = x

r1 :: [a] -> [a]
r1 = tail

r2 :: [a] -> [a]
r2 = reverse

co :: (b -> c) -> (a -> b) -> a -> c
co f g = f . g

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f x = f x
