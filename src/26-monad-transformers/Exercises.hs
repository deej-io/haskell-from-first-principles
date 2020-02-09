{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Ch26.Exercises where

import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.State as S
import Data.Functor.Identity
import Data.IORef
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import System.Environment (getArgs)
import Web.Scotty.Trans

rDec :: Num a => Reader a a
rDec = subtract 1 <$> ask

rShow :: Show a => ReaderT a Identity String
rShow = show <$> ask

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  v <- ask
  liftIO $ putStrLn ("Hi: " ++ show v)
  return $ v + 1

sPrintIncAccum :: (Num a, Show a) => S.StateT a IO String
sPrintIncAccum = do
  v <- S.get
  let vStr = show v
  liftIO $ putStrLn ("Hi: " ++ vStr)
  S.put (v + 1)
  return vStr

isValid :: String -> Bool 
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- liftIO getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)

data Config = Config {
  counts :: IORef (M.Map Text Integer),
  prefix :: Text
}

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = 
  let (oldValue, newMap) = M.insertLookupWithKey (\_ _ o -> o + 1) k 1 m
   in (newMap, fromMaybe 1 oldValue)

app :: Scotty ()
app =
  get "/:key" $ do
    config :: Config <- lift ask
    counter :: M.Map Text Integer <- liftIO . readIORef . counts $ config
    unprefixed <- param "key"
    let key' = mappend (prefix config) unprefixed
    let (newMap, newInteger) = bumpBoomp key' counter
    liftIO $ writeIORef (counts config) newMap
    html $
      mconcat ["<h1>Success! Count was: " , TL.pack $ show newInteger, "</h1>"]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = runReaderT r config
  scottyT 3000 runR app

