{-# LANGUAGE OverloadedStrings #-}
module ScottyMaybeT where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Web.Scotty

param' :: Parsable a => Text -> MaybeT ActionM a
param' k = MaybeT $
  rescue (Just <$> param k)
         (const (return Nothing))

type Reco = (Integer, Integer, Integer, Integer)

main = scotty 3000 $
  get "/:word" $ do
    beam' <- runMaybeT $ param' "word"

    let beam = fromMaybe "" beam'

    reco <- runMaybeT $ do
      a <- param' "1"
      liftIO $ print a

      b <- param' "2"
      c <- param' "3"
      d <- param' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)

    liftIO $ print reco
    html $
      mconcat ["<h1>Scotty, ",
               beam,
               " me up!</h1>"]
