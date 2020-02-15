module Main where

import Criterion.Main
import Control.Monad
import Control.Monad.Trans.State
import Data.Maybe
import Data.Sequence

class Queueable c where
  push :: a -> c a -> c a
  pop :: c a -> Maybe (a, c a)

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

queue :: Queue a
queue = Queue [] []

instance Queueable Queue where
  push x (Queue e d) = Queue (x:e) d

  pop (Queue [] []) = Nothing
  pop (Queue es (d:ds)) = Just (d, Queue es ds)
  pop (Queue es []) = pop $ Queue [] (Prelude.reverse es)

instance Queueable [] where
  push x xs = xs ++ [x]

  pop [] = Nothing
  pop (x:xs) = Just (x, xs)

instance Queueable Seq where
  push x xs = x :<| xs
  pop (xs:|>x) = Just (x, xs)
  pop Empty = Nothing

nth :: Queueable q => Int -> q a -> Maybe (a, q a)
nth 0 q = pop q
nth n q = case pop q of
  Nothing -> Nothing
  Just (_, q) -> nth (n - 1) q

pushPushPop :: Queueable q => a -> State (q a) a
pushPushPop a = do
  modify (push a)
  modify (push a)
  popped <- gets pop
  case popped of
    Just (a, q) -> do
      put q
      return a

unsafePopState :: Queueable q => State (q a) a
unsafePopState = do
  (a, q) <- fromJust <$> gets pop
  put q
  return a
  

pushPushPushPopPushPop :: Queueable q => a -> State (q a) a
pushPushPushPopPushPop a = do
  modify (push a)
  modify (push a)
  modify (push a)
  unsafePopState
  modify (push a)
  unsafePopState

main = defaultMain [
    bench "push and nth queue" $ whnf (nth 1000 . foldr push queue) [1..9999],
    bench "push and nth list" $ whnf (nth 1000 . foldr push []) [1..9999],
    bench "push and nth seq" $ whnf (nth 1000 . foldr push Empty) [1..9999],
    bench "push push pop queue" $ whnf (runState (traverse pushPushPop [1..9999])) queue,
    bench "push push pop list" $ whnf (runState (traverse pushPushPop [1..9999])) [],
    bench "push push pop seq" $ whnf (runState (traverse pushPushPop [1..9999])) Empty,
    bench "push push push pop push pop queue" $ whnf (runState (traverse pushPushPushPopPushPop [1..9999])) queue,
    bench "push push push pop push pop list" $ whnf (runState (traverse pushPushPushPopPushPop [1..9999])) [],
    bench "push push push pop push pop list" $ whnf (runState (traverse pushPushPushPopPushPop [1..9999])) Empty
  ]
