module Ch23.RollYourOwn where

import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN n = go 0 (0, [])
  where
    go :: Int -> (Int, [Die]) -> StdGen -> (Int, [Die])
    go sum (count, rolls) gen
      | sum >= n = (count, rolls)
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
           in go (sum + die) (count + 1, intToDie die : rolls) nextGen
