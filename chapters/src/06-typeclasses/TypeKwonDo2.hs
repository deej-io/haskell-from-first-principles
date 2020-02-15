module TypeKwonDo2 where

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk fn x y = fn x == y

arith :: Num b => (a -> b) -> Integer -> a -> b
arith fn x y = (fn y) + fromIntegral x
