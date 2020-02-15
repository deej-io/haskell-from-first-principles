{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Control.Monad                  ( forever
                                                , when
                                                )
import           Data.Char                      ( toLower )
import           Data.Maybe                     ( isJust )
import           Data.List                      ( intersperse )
import           System.Exit                    ( exitSuccess )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           System.Random                  ( randomRIO )
import           Text.RawString.QQ


type WordList = [String]

stages :: [String]
stages =
  [ [r|







|]
  , [r|






  ________
|]
  , [r|

   |
   |
   |
   |
   |
  _|_______
|]
  , [r|

   |
   |
   |
   |
   |
  _|\______
|]
  , [r|
   ______
   |
   |
   |
   |
   |
  _|\______
|]
  , [r|
   ______
   |/
   |
   |
   |
   |
  _|\______
|]
  , [r|
   ______
   |/   |
   |
   |
   |
   |
  _|\______
|]
  , [r|
   ______
   |/   |
   |
   |
   |         0
   |        /|\
  _|\______ / \
|]
  , [r|
   ______
   |/   |
   |   \0/
   |    |
   |   / \
   |
  _|\______
|]
  ]

allWords :: IO WordList
allWords = lines <$> readFile "data/dict.txt"

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = filter gameLength <$> allWords
 where
  gameLength w = let l = length w in l >= minWordLength && l < maxWordLength

randomWord' :: WordList -> IO String
randomWord' wl = do
  randomIndex <- randomRIO (0, length wl)
  return $ wl !! randomIndex

randomWord :: IO String
randomWord = gameWords >>= randomWord'

data Puzzle = Puzzle String [Maybe Char] String

instance Show Puzzle where
  show puzzle@(Puzzle _ discovered guessed) =
    stages
      !! errors puzzle
      ++ intersperse ' ' (fmap renderPuzzleChar discovered)
      ++ "\nGuessed so far: "
      ++ guessed
   where
    renderPuzzleChar (Just c) = c
    renderPuzzleChar _        = '_'

mkPuzzle :: String -> Puzzle
mkPuzzle word = Puzzle word (replicate (length word) Nothing) ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = c `elem` word
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = c `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word
                                                         newFilledInSoFar
                                                         (c : s)
 where
  zipper guess wordChar guessChar =
    if wordChar == guess then Just wordChar else guessChar
  newFilledInSoFar = zipWith (zipper c) word filledInSoFar

errors :: Puzzle -> Int
errors (Puzzle _ discovered guessed) =
  length guessed - length (filter isJust discovered)

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again"
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ guessed) = when (errors puzzle > 7) $ do
  putStrLn $ stages !! 8
  putStrLn "You lose!"
  putStrLn $ "The word was: " ++ wordToGuess
  exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word filledInSoFar _) = when (all isJust filledInSoFar) $ do
  putStrLn "You win!"
  putStrLn $ "The word was: " ++ word
  exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn "Current puzzle is: "
  print puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord
  let puzzle = mkPuzzle (fmap toLower word)
  runGame puzzle

