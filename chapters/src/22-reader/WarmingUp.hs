module Ch22.WarmingUp where

import Data.Char

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = (,) <$> rev <*> cap

tupledM :: String -> (String, String)
tupledM = do
  r <- rev
  c <- cap
  return (r, c)

tupledM' :: String -> (String, String)
tupledM' = rev >>= \r -> cap >>= \c -> return (r, c)

