module Test where

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

triple :: Num x => x -> x
triple x = x * 3
