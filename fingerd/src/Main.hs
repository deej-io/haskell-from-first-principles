{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import           Control.Monad                  ( forever )
import           Data.Typeable
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.List                      ( intersperse )
import           Control.Exception
import           Network.Socket          hiding ( recv )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import qualified Data.Text                     as T
import           Database.SQLite.Simple  hiding ( close, bind )
import qualified Database.SQLite.Simple        as SQLite
import           Database.SQLite.Simple.Types
import           Text.RawString.QQ

data User =
  User {
    userId :: Integer,
    username :: Text,
    shell :: Text,
    homeDirectory :: Text,
    realName :: Text,
    phone :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User id username shell homeDir realName phone) =
    toRow (id, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * FROM users"

getUserQuery :: Query
getUserQuery = "SELECT * FROM users WHERE username = ?"

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)

  case results of
    []     -> return Nothing
    [user] -> return $ Just user
    _      -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow

  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn

 where
  meRow :: UserRow
  meRow =
    (Null, "djr", "/bin/zsh", "/home/djr", "Daniel Rollins", "0745238482")


returnUsers :: ReaderT Config IO ()
returnUsers = do
  conn <- asks dbConn
  sock <- asks sock

  liftIO $ do
    rows <- query_ conn allUsers

    let usernames        = map username rows
        newlineSeparated = T.concat $ intersperse "\n" usernames

    sendAll sock (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  [ "Login: ", e username, "\t\t\t\t"
  , "Name: ", e realName, "\n"
  , "Directory: ", e homeDir, "\t\t\t"
  , "Shell: ", e shell, "\n"
  ]
  where e = encodeUtf8

returnUser :: Text -> ReaderT Config IO ()
returnUser username = do
  dbConn <- asks dbConn
  sock <- asks sock

  liftIO $ do
    maybeUser <- getUser dbConn (T.strip username)
    case maybeUser of
      Nothing -> putStrLn $ "Couldn't find matching user for username: " ++ show username
      Just user -> sendAll sock (formatUser user)

handleQuery :: ReaderT Config IO ()
handleQuery = do
  sock <- asks sock

  msg <- liftIO $ recv sock 1024

  case msg of
    "\r\n" -> returnUsers
    name   -> returnUser (decodeUtf8 name)

handleQueries :: ReaderT Config IO ()
handleQueries = do
  sock <- asks sock
  conn <- asks dbConn

  liftIO . forever $ do
    (sock, _) <- accept sock
    putStrLn "Got connection, handling query"

    runReaderT handleQuery $ Config conn sock
    close sock

data Config = Config {
  dbConn :: Connection,
  sock :: Socket
}

main :: IO ()
main = withSocketsDo $ do
  addrInfos <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] })) Nothing (Just "79")
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 1

  conn <- open "finger.db"

  let config = Config conn sock

  runReaderT handleQueries config
  SQLite.close conn
  close sock
