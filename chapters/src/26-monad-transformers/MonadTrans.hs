module MonadTrans where

import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Control.Monad (liftM)

class MonadTrans' t where
  lift :: (Monad m) => m a -> t m a

instance MonadTrans' (ExceptT e) where
  lift = ExceptT . fmap Right

instance MonadTrans' (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)
