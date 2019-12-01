module QuickCheckEx where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

listOrdered :: (Foldable t, Ord a) => t a -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, _) = (Just y, x >= y)

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

multiplyAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multiplyAssociative x y z = x * (y * z) == (x * y) * z

multiplyCommutative :: (Eq a, Num a) => a -> a -> Bool
multiplyCommutative x y = x * y == y * x

quotRemIdentity x y = quot x y * y + rem x y == x

divModIdentity x y = div x y * y + mod x y == x

main :: IO ()
main = hspec $ do
  describe "half" $
    it "" $ property $
      \x -> halfIdentity x == (x :: Double)

  describe "sort" $ 
    it "produces an ordered list" $ property $
      \x -> listOrdered . sort $ (x :: [Int])

  describe "plus" $ do
    it "is associative" $ property $
      \x y z -> plusAssociative x y (z :: Int)
    it "is commutative" $ property $
      \x y -> plusCommutative x (y :: Int)

  describe "plus" $ do
    it "is associative" $ property $
      \x y z -> plusAssociative x y (z :: Int)
    it "is commutative" $ property $
      \x y -> plusCommutative x (y :: Int)

  describe "multiply" $ do
    it "is associative" $ property $
      \x y z -> multiplyAssociative x y (z :: Int)
    it "is commutative" $ property $
      \x y -> multiplyCommutative x (y :: Int)

  describe "quot rem" $
    it "have an identity relationship" $ property $
      \x y -> quotRemIdentity (getPositive x) (getPositive y :: Int)

  describe "div mod" $
    it "have an identity relationship" $ property $
      \x y -> divModIdentity (getPositive x) (getPositive y :: Int)
