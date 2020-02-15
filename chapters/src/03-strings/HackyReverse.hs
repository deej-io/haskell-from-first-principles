module HackyReverse where

-- expect to only use drop and take to reverse Curry is awesome word-wise

rvrs :: String -> String
rvrs x = concat [awesome, " ", is, " ", curry]
    where awesome = drop 9 x
          is      = take 2 $ drop 6 x
          curry   = take 5 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
