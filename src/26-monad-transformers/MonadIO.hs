module MonadIO where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

class Monad m => MonadIO m where
  liftIO :: IO a -> m a

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = MaybeT . fmap Just . liftIO 

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO m = ReaderT $ \r -> liftIO m

instance MonadIO m => MonadIO (StateT s m) where
  liftIO m = StateT $ \s -> do
    a <- liftIO m
    return (a, s)
