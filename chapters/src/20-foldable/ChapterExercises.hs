module Ch20.ChapterExercises where

newtype Constant a b = Constant b

instance Foldable (Constant a) where
  foldMap m (Constant b) = m b

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap m (Two _ b) = m b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap m (Three _ _ c) = m c

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap m (Three' _ b1 b2) = m b1 <> m b2

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap m (Four' _ b1 b2 b3) = m b1 <> m b2 <> m b3

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)
