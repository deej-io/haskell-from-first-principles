module UnderstandingFolds where

ex1 = (foldr (*) 1 [1..5]) == (foldl (*) 1 [1..5])
  && (foldr (*) 1 [1..5]) == (foldl (flip (*)) 1 [1..5])

-- a) foldr (++) ["woot", "WOOT", "woot"]
ex5a = foldr (++) "" ["woot", "WOOT", "woot"]
-- b) foldr max [] "fear is the little death"
ex5b = foldr max 'a' "fear is the little death"
-- c) foldr and True [False, True]
ex5c = foldr (&&) True [False, True]
-- d) This one is more subtle than the previous. Can it ever return a different answer?
--    foldr (||) True [False, True]
ex5d = foldr (||) False [False, True]
-- e) foldl ((++) . show) "" [1..5]
ex5e = foldr ((++) . show) "" [1..5]
-- f) foldr const 'a' [1..5]
ex5f = foldl const 'a' [1..5]
-- g) foldr const 0 "tacos"
ex5g = foldl const 0 "tacos"
-- h) foldl (flip const) 0 "burritos"
ex5h = foldr (flip const) 0 "burritos"
-- i) foldl (flip const) 'z' [1..5]
ex5i = foldr (flip const) 'z' [1..5]
