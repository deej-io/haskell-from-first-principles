module DoesntWork where

bindExp :: Integer -> String
bindExp x =
  let y = 5 in
    "the integer was: " ++ show x
    ++ " and y was: " ++ show y

-- Doesn't work because z doesn't know what y is
--
--  bindExp' :: Integer -> String
--  bindExp' x =
--    let z = y + x in
--      let y = 5 in
--        "the integer was: " ++ show x
--        ++ " and y was: " ++ show y
--        ++ " and z was: " ++ show z

bindExp' :: Integer -> String
bindExp' x =
  let y = 5 in
    let z = y + x in
      "the integer was: " ++ show x
      ++ " and y was: " ++ show y
      ++ " and z was: " ++ show z
