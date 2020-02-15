module Print3Broken where

greeting :: String
greeting = "Yarrrrrr"

printSecond :: IO ()
printSecond = do
    putStrLn greeting

main :: IO ()
main = do
    putStrLn greeting
    printSecond
