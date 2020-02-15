module Main where

import           Control.Monad                  ( forever )
import           Network.Socket          hiding ( recv )
import           Network.Socket.ByteString      ( recv
                                                , sendAll
                                                )

logAndEcho :: Socket -> IO ()
logAndEcho sock = forever $ do
  (conn, _) <- accept sock
  printAndKickback conn
  close conn

 where
  printAndKickback conn = do
    msg <- recv conn 1024
    print msg
    sendAll conn msg

main :: IO ()
main = withSocketsDo $ do
  addrInfos <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                           Nothing
                           (Just "79")
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 1
  logAndEcho sock
  close sock


