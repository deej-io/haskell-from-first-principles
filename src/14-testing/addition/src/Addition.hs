module Addition where

import           Test.Hspec
import           Test.QuickCheck

multiply :: (Eq a, Num a) => a -> a -> a
multiply x y | x == 0 || y == 0 = 0
             | otherwise        = go x y
 where
  go x' y' | x' == 1   = y'
           | y' == 1   = x'
           | otherwise = go (x' - 1) (y' + y)

main :: IO ()
main = hspec $ describe "multiply" $ do
  it "evaluates to 0 if x is 0" $ multiply 0 2 `shouldBe` (0 :: Int)
  it "evaluates to 0 if y is 0" $ multiply 2 0 `shouldBe` (0 :: Int)
  it "evaluates to y if x is 1" $ multiply 2 0 `shouldBe` (0 :: Int)
  it "evaluates to y if x is 1" $ multiply 1 10 `shouldBe` (10 :: Int)
  it "evaluates to x if y is 1" $ multiply 10 1 `shouldBe` (10 :: Int)
  it "multiply 3 7 == 21" $ multiply 3 7 `shouldBe` (21 :: Int)

  it "x + 1 is always greater than x" $ property $ \x -> x + 1 > (x :: Int)
