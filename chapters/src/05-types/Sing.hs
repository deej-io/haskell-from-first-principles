module Sing where

fstString :: [Char] -> [Char]
fstString = (++ " in the rain")

sndString :: [Char] -> [Char]
sndString = (++ " over the rainbow")

sing = if (x > y) then fstString x else sndString x
    where x = "Singin"
          y = "Somehwhere"
