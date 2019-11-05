module Main where
import           Control.Monad                  ( forever
                                                , when
                                                )
import           Data.Traversable               ( traverse )
import           Morse                          ( stringToMorse
                                                , morseToChar
                                                )
import           System.Environment             ( getArgs )
import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import           System.IO                      ( getLine
                                                , isEOF
                                                )


failLine :: String -> IO ()
failLine line = do
  putStrLn $ "ERROR: " ++ line
  exitFailure

encodeLine :: String -> IO ()
encodeLine line = do
  let morse = stringToMorse line
  case morse of
    (Just str) -> putStrLn (unwords str)
    Nothing    -> failLine line

decodeLine :: String -> IO ()
decodeLine line = do
  let decoded :: Maybe String
      decoded = traverse morseToChar (words line)
  case decoded of
    (Just s) -> putStrLn s
    Nothing  -> failLine line

convertStdin :: (String -> IO a) -> IO b
convertStdin codec = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess
  getLine >>= codec

convertToMorse :: IO ()
convertToMorse = convertStdin encodeLine

convertFromMorse :: IO ()
convertFromMorse = convertStdin decodeLine

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] -> case arg of
      "from" -> convertFromMorse
      "to"   -> convertToMorse
      _      -> argError
    _ -> argError
 where
  argError = do
    putStrLn
      "Please specify the\
            \ first argument\
            \ as being 'from' or\
            \ 'to' morse,\
            \ such as: morse to"
    exitFailure
