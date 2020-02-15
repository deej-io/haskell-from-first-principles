module Main where

import Vigenere hiding (main)

import System.Environment (getArgs)
import System.IO
import System.Exit

type Mode = String -> String -> String

parseMode :: String -> IO Mode
parseMode mode = case mode of
  "-e" -> return cipher
  "-d" -> return uncipher
  _    -> do
    hPutStrLn stderr "Mode must be '-e' or '-d'"
    exitFailure

parseArgs :: IO (Mode, String)
parseArgs = do 
  args <- getArgs
  if length args /= 2
    then exitFailure
    else do
      let [mode, key] = args
      mode <- parseMode mode
      return (mode, key)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  (mode, key) <- parseArgs
  message <- getLine
  print message
  print $ mode key message
