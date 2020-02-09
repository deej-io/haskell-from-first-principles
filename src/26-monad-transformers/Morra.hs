module Morra where

import System.Random
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

data Scores = Scores {
  player :: Int,
  cpu :: Int
} deriving (Eq, Show)

mkScores :: Scores
mkScores = Scores 0 0

data Turn = Turn {
  fingers :: Int,
  guess :: Int
} deriving Show

validateFingers :: Monad m => Int -> ExceptT String m Int
validateFingers = undefined

determineWinner :: Monad m => Turn -> Turn -> StateT Scores m ()
determineWinner (Turn p1f p1g) (Turn p2f p2g)
  | p1g == p2g = return ()
  | p1g == total = modify $ \s -> Scores (player s + total) (cpu s)
  | p2g == total = modify $ \s -> Scores (player s) (cpu s + total)
  | otherwise = return ()
    where total = p1f + p2f

playRound :: StateT Scores IO ()
playRound = do
  p1 <- liftIO $ do
    putStrLn "How many fingers?"
    fingers <- read <$> getLine :: IO Int
    fingers <- runExceptT validateFingers fingers
    putStrLn "What's your guess?"
    guess <- read <$> getLine :: IO Int
    return $ Turn fingers guess
  p2 <- liftIO $ do
    fingers <- randomRIO (1, 3) :: IO Int
    guess <- randomRIO (1, 6) :: IO Int
    return $ Turn fingers guess

  liftIO $ do
    putStrLn $ "player 1: " ++ show p1
    putStrLn $ "player 2: " ++ show p2

  determineWinner p1 p2

main :: IO ()
main = do
  s <- execStateT playRound mkScores
  print s
