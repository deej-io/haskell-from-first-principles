module CasePractice where

functionC x y = if (x > y) then x else y

functionC' x y = case (x > y) of
  True  -> x
  False -> y

ifEvenAdd2 n = if even n then (n + 2) else n

ifEvenAdd2' n = case even n of
  True  -> n + 2
  False -> n

num x =
  case compare x 0 of
    EQ -> 0
    LT -> -1
    GT -> 1
