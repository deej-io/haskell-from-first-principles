module Main where

import           Criterion.Main

infixl 9 !?
(!?) :: [a] -> Int -> Maybe a
_ !? n | n < 0 = Nothing
[]       !? _  = Nothing
(x : _ ) !? 0  = Just x
(_ : xs) !? n  = xs !? (n - 1)

aList :: [Int]
aList = [1 .. 9999]

main :: IO ()
main = defaultMain
  [ bench "index list 9999" $ whnf (aList !!) 9998
  , bench "index list maybe 9999" $ whnf (aList !?) 9998
  ]
