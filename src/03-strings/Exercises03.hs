module Exercises03 where

-- Building functions
ex1 = drop 1

ex2a = flip (++) "!"
ex2b = flip (!!) 4
ex2c = drop 9

-- 3)
thirdLetter :: String -> Char
thirdLetter = flip (!!) 3
-- 4)
letterIndex :: Int -> Char
letterIndex = (!!) "Curry is awesome!"
