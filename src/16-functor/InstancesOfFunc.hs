module InstancesOfFunc where

import           Test.QuickCheck
import           Data.Void

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2)

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)  where
    arbitrary = Two <$> arbitrary <*> arbitrary

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)


data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b )


-- Tests

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)  where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d)  where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose (Fn f) (Fn g) x = (fmap g (fmap f x)) == (fmap (g . f) x)

type FunctorIdentityTest f = f Int -> Bool
type FunctorComposeTests f
    = (Fun String Int) -> (Fun Int Char) -> f String -> Bool

main :: IO ()
main = do
    quickCheck (functorIdentity :: FunctorIdentityTest Identity)
    quickCheck (functorCompose :: FunctorComposeTests Identity)
    quickCheck (functorIdentity :: FunctorIdentityTest Pair)
    quickCheck (functorCompose :: FunctorComposeTests Pair)
    quickCheck (functorIdentity :: FunctorIdentityTest (Two Int))
    quickCheck (functorCompose :: FunctorComposeTests (Two Int))
    quickCheck (functorIdentity :: FunctorIdentityTest (Three Int Int))
    quickCheck (functorCompose :: FunctorComposeTests (Three Int Int))
    quickCheck (functorIdentity :: FunctorIdentityTest (Four Int Int Int))
    quickCheck (functorCompose :: FunctorComposeTests (Four Int Int Int))
    quickCheck (functorIdentity :: FunctorIdentityTest (Four' Int))
    quickCheck (functorCompose :: FunctorComposeTests (Four' Int))
