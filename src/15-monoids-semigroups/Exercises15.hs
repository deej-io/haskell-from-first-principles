module Exercises15 where

import           Data.Monoid
import           Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a)  where
  (Identity a) <> (Identity b) = Identity $ a <> b

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Semigroup a, Semigroup b) => Semigroup (Two a b)  where
  (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b)  where
  arbitrary = Two <$> arbitrary <*> arbitrary

data Three a b c = Three a b c deriving (Eq, Show)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c)  where
  mempty = Three mempty mempty mempty

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c)  where
  (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)


instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c)  where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d)  where
  (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) =
    Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d)  where
  mempty = Four mempty mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d)  where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _               <> _               = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _               = BoolDisj True
  _               <> (BoolDisj True) = BoolDisj True
  _               <> _               = BoolDisj False

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst f1 <> Fst f2 = Fst f2
  Snd s  <> _      = Snd s
  _      <> Snd s  = Snd s

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = oneof [Fst <$> arbitrary, Snd <$> arbitrary]

newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine $ g <> f

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ const mempty

newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f . g

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = Comp id

data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success' b)  <> _             = Success' b
  _             <> (Success' b ) = Success' b
  (Failure' a1) <> (Failure' a2) = Failure' $ a1 <> a2

instance Monoid a => Monoid (Validation a b) where
  mempty = Failure' mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary =
    frequency [(10, Failure' <$> arbitrary), (1, Success' <$> arbitrary)]

newtype Mem s a = Mem { runMem :: s -> (a, s) }

combineMemFuncs f g x =
  let (a, b) = f x
      (c, d) = g b
  in  (a <> c, d)

instance (Semigroup a) => Semigroup (Mem s a) where
  (Mem f) <> (Mem g) = Mem $ combineMemFuncs f g

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty  = Mem $ (,) mempty
  mappend = (<>)

-- tests

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

type FunWrapperAssocTest a b = Fun a b -> Fun a b -> Fun a b -> a -> Bool
type FunWrapperIdentityTest a b = Fun a b-> a-> Bool


funWrapperAssocTest
  :: (Eq b, Semigroup s)
  => (s -> (a -> b))
  -> ((a -> b) -> s)
  -> FunWrapperAssocTest a b
funWrapperAssocTest unwrapper wrapper (Fn f) (Fn g) (Fn h) a =
  let left  = unwrapper ((wrapper f <> wrapper g) <> wrapper h)
      right = unwrapper (wrapper f <> (wrapper g <> wrapper h))
  in  left a == right a

funWrapperLeftIdentityTest :: (Eq b, Monoid m) => (m -> (a -> b)) -> ((a -> b) -> m) -> FunWrapperIdentityTest a b
funWrapperLeftIdentityTest unwrapper wrapper (Fn f) a =
  unwrapper (mempty <> wrapper f) a == f a

funWrapperRightIdentityTest :: (Eq b, Monoid m) => (m -> (a -> b)) -> ((a -> b) -> m) -> FunWrapperIdentityTest a b
funWrapperRightIdentityTest unwrapper wrapper (Fn f) a =
  unwrapper (wrapper f <> mempty) a == f a


type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdentityAssoc = Identity [Int] -> Identity [Int] -> Identity [Int] -> Bool
type TwoAssoc = Two String [Int] -> Two String [Int] -> Two String [Int] -> Bool

type ThreeAssoc
  =  Three String [Int] (Sum Int)
  -> Three String [Int] (Sum Int)
  -> Three String [Int] (Sum Int)
  -> Bool

type FourAssoc
  =  Four String [Int] (Sum Int) (Product Int)
  -> Four String [Int] (Sum Int) (Product Int)
  -> Four String [Int] (Sum Int) (Product Int)
  -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

type ValidationAssoc
  = Validation [Int] Int -> Validation [Int] Int -> Validation [Int] Int -> Bool

test :: IO ()
test = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Identity [Int] -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two [Int] [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Two [Int] [Int] -> Bool)

  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (monoidLeftIdentity :: Three [Int] [Int] [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Three [Int] [Int] [Int] -> Bool)

  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (monoidLeftIdentity :: Four [Int] [Int] [Int] [Int] -> Bool)
  quickCheck (monoidRightIdentity :: Four [Int] [Int] [Int] [Int] -> Bool)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  quickCheck (semigroupAssoc :: OrAssoc)

  quickCheck (semigroupAssoc :: ValidationAssoc)
  quickCheck (monoidLeftIdentity :: Validation [Int] Int -> Bool)
  quickCheck (monoidRightIdentity :: Validation [Int] Int -> Bool)

  quickCheck
    (funWrapperAssocTest unCombine Combine :: FunWrapperAssocTest Int [Int])
  quickCheck
    (funWrapperLeftIdentityTest unCombine Combine :: FunWrapperIdentityTest Int [Int])
  quickCheck
    (funWrapperRightIdentityTest unCombine Combine :: FunWrapperIdentityTest Int [Int])
  
  quickCheck
    (funWrapperAssocTest unComp Comp :: FunWrapperAssocTest [Int] [Int])
  quickCheck
    (funWrapperLeftIdentityTest unComp Comp :: FunWrapperIdentityTest [Int] [Int])
  quickCheck
    (funWrapperRightIdentityTest unComp Comp :: FunWrapperIdentityTest [Int] [Int])

  quickCheck
    (funWrapperAssocTest runMem Mem :: FunWrapperAssocTest Int ([Int], Int))
  quickCheck
    (funWrapperLeftIdentityTest runMem Mem :: FunWrapperIdentityTest Int ([Int], Int))
  quickCheck
    (funWrapperRightIdentityTest runMem Mem :: FunWrapperIdentityTest Int ([Int], Int))
