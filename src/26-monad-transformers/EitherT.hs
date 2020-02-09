module EitherT where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT fab) <*> (EitherT ma) = EitherT $ (<*>) <$> fab <*> ma

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT ma) >>= f = EitherT $ do
    v <- ma
    case v of
      Left e -> return $ Left e
      Right a -> runEitherT (f a)

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ fmap swap ma
  where 
    swap (Left e) = Right e
    swap (Right a) = Left a

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT ma) = ma >>= apply
  where
    apply (Left a) = f a
    apply (Right b) = g b
