{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Plus
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Directory
import System.Environment
import System.IO
import System.ZMQ4.Monadic
import Text.Read (readMaybe)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import qualified Data.ByteString.Lazy as BSL


import Message


-- Request transport.
reqTp :: String
reqTp = "tcp://127.0.0.1:5555"

-- Reply transport.
repTp :: String
repTp = "tcp://127.0.0.1:5556"


-- Push socket.
pushSocket :: String -> ZMQ z (Socket z Pub)
pushSocket addr =
  -- Use Pub to avoid blocking.
  do sock <- socket Pub
     bind sock addr
     pure sock

-- Pull socket.
pullSocket :: String -> ZMQ z (Socket z Sub)
pullSocket addr =
  -- Use Sub to listen to Pub.
  do sock <- socket Sub
     connect sock addr
     subscribe sock ""
     pure sock


-- Request handler.

processRequest :: Request -> IO Reply

processRequest (EchoReq data_) =
  do putStrLn "ECHO"
     pure $ Answer (Just False) (Just "OK") $ EchoRepData data_
     
processRequest (ReadReq fileName) =
  do putStrLn "READ"
     fileExists <- doesFileExist (T.unpack fileName)
     if (not fileExists)
      then pure $
        Answer (Just True) (Just "read file error") $ ReadRepData ""
      else do
        h <- openFile (T.unpack fileName) ReadMode
        Answer (Just False) (Just "OK") .
          ReadRepData . T.pack <$> hGetContents h
          
processRequest (WriteReq fileName contents) =
  do putStrLn "WRITE"
     T.writeFile (T.unpack fileName) contents
     pure $ Answer (Just False) (Just "OK") WriteRepData
          

-- Server.

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero pure

server :: IO ()
server = runZMQ $
  do reqPull <- pullSocket reqTp
     repPush <- pushSocket repTp
     forM_ [0..100] $ \ i -> do
       liftIO $ putStrLn $ "Iteration " ++ (show i)
       poll 100 $
         [Sock reqPull [In]
         $ Just $
           \_ -> do
             buffer <- receive reqPull
             -- Ignore ill-formated requests.
             _      <- runMaybeT $
               do req <- liftMaybe $ parseRequest' buffer
                  rep <- liftIO    $ processRequest req
                  lift $ send repPush [] $ encodeReply' rep 
             pure ()
         ]


-- Client (I'm too lazy to write this too).
-- It just sends ECHO request and waits for reply.
client :: IO ()
client = runZMQ $
  do reqPush <- pushSocket reqTp
     repPull <- pullSocket repTp
     
     let req = EchoReq "Hello World!"
     
     liftIO $ print (encodeRequest' req)
     
     let req' = parseRequest' (encodeRequest' req)
     
     liftIO $ print req'
     
     send reqPush [] $ encodeRequest' req 
     
     pure ()


-- Main.

main :: IO ()
main = do
  args <- map readMaybe <$> getArgs
  case args of
    [mWorkType] ->
      case mWorkType of
        Nothing -> printHelp
        Just wt ->
          case wt of
            0 -> putStrLn "Starting server..." >> server
            1 -> putStrLn "Starting client..." >> client
            _ -> putStrLn "Unsupported type of work. Quit." >> pure ()
    _ -> printHelp
  
  where
    printHelp = do
      putStrLn "Expected 0 or 1: "
      putStrLn "  0 - work as server;"
      putStrLn "  1 - work as client;"
