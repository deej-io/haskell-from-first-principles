module Main where

import Control.Monad

f :: IO ()
f = do
  print ([1..] !! 999999)
  putStrLn "f"

g :: IO ()
g = do
  print ([1..] !! 9999999)
  putStrLn "g"

blah :: [Integer]
blah = [1..1000]

main :: IO ()
main = f >> g >> replicateM_ 10000 (print blah)
