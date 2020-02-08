module IdentityT where

newtype IdentityT f a = IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative m => Applicative (IdentityT m) where
  pure = IdentityT . pure
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad m => Monad (IdentityT m) where
  return = pure
  ma >>= f = IdentityT $ runIdentityT ma >>= runIdentityT . f
