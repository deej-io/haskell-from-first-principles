module ModifyingCode where

import           Control.Monad
import           System.Exit
import           Data.Char

normalize :: String -> String
normalize = fmap toLower . filter isAlpha

palindrome :: IO ()
palindrome = forever $ do
  line1 <- fmap normalize getLine
  if line1 /= reverse line1
    then do
      putStrLn "Nope!"
      exitSuccess
    else putStrLn "It's a palindrome!"
