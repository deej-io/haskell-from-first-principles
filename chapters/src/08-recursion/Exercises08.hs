module Exercises08 where

-- Review of types
ex1_1 :: [[Bool]]
ex1_1 = [[True, False], [True, True], [False, True]]

ex1_2 :: [[Bool]]
ex1_2 = [[3==3], [6 > 5], [3 < 4]]

func :: [a] -> [a] -> [a]
func x y = x ++y

ex1_3 = func "Hello" "World"

-- Reviewing Currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

ex2_1 = appedCatty "woohoo!" -- "woops mrow woohoo!"

ex2_2 = frappe "1" -- "1 mrow haha"

ex2_3 = frappe (appedCatty "2") -- "woops mrow 2 mrow haha"

ex2_4 = appedCatty (frappe "blue") -- "woops mrow blue mrow haha"

ex2_5 = cattyConny (frappe "pink")
                   (cattyConny "green" (appedCatty "blue"))
      -- "pink mrow haha mrow green mrow woops mrow blue"

ex2_6 = cattyConny (flippy "Pugs" "are") "awesome" -- "are mrow Pugs mrow awesome"

-- Recursion

-- 1) dividedBy 15 2 = 15 - 2
--                     13 - 2
--                     11 - 2
--                      9 - 2
--                      7 - 2
--                      5 - 2
--                      3 - 2
--                      1 - 2
--                      (7, 1)

recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum n = go 1
  where go i
         | n == i = i
         | otherwise = i + go (i + 1)


data DividedByResult = Result Integer  | DivideByZero
  deriving Show

dividedBy :: Integer -> Integer -> DividedByResult
dividedBy _ 0 = DivideByZero
dividedBy num denom
  | signum denom == signum num = Result . go num denom $ 0
  | otherwise = Result . negate . go (abs num) (abs denom) $ 0
    where go n   d count
           | n < d = count
           | otherwise =
               go (n - d) d (count + 1)

mc91 :: (Ord a, Num a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 . (+11) $ n
