{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language DeriveGeneric, DeriveAnyClass #-}
module CloudyRolly where

import Control.Concurrent.STM
import Control.Monad (replicateM)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Text (Text)
import qualified Data.Text
import GHC.Generics
import Network.Simple.TCP
import System.Random

type GameCode = Text

data Request 
  = NewGame
  | JoinGame GameCode
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Response
  = GameCode GameCode
  | GameNotFound
  | GameStarts
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type State = HashMap GameCode [Socket]

crServer :: IO ()
crServer = do
  state <- newTVarIO @State Map.empty
  serve "127.0.0.1" "8080" $ \(skt, _) ->
    crServerWorker state skt

crServerWorker :: TVar State -> Socket -> IO ()
crServerWorker state skt = start
  where
    start = do
      Just req <- recvJson skt
      case req of
        NewGame -> newGame
        JoinGame code -> joinGame code

    newGame = do
      code <- randomCode
      atomically $
        modifyTVar state (Map.insert code [skt])
      sendJson skt (GameCode code)
      atomically $ do
        Just skts <- Map.lookup code <$> readTVar state
        check (length skts > 1)
      sendJson skt GameStarts
      play

    joinGame code = do
      found <- atomically $ do
        result <- Map.lookup code <$> readTVar state
        case result of
          Nothing  -> pure False
          Just skts -> do
            modifyTVar state (Map.insertWith (<>) code [skt])
            pure True
      if found
         then sendJson skt GameStarts >> play
         else sendJson skt GameNotFound

    play = putStrLn "play!"

player1Client :: IO ()
player1Client = connect "127.0.0.1" "8080" $ \(skt, _) -> do
  sendJson skt NewGame
  Just (GameCode code) <- recvJson skt
  putStrLn $ "code: " <> Data.Text.unpack code
  Just GameStarts <- recvJson skt
  putStrLn "game starts!"

player2Client :: GameCode -> IO ()
player2Client code = connect "127.0.0.1" "8080" $ \(skt, _) -> do
  sendJson skt (JoinGame code)
  Just GameStarts <- recvJson skt
  putStrLn "game starts!"

-- UTILITIES
-- =========

instance MonadFail STM where
  fail _ = retry

randomCode :: (MonadIO m) => m Text
randomCode = liftIO $ 
  Data.Text.pack <$> replicateM 4 (randomRIO ('A', 'Z'))

recvJson :: (MonadIO m, FromJSON a) => Socket -> m (Maybe a)
recvJson skt = do
  line <- recvLine skt
  case line of
    Nothing -> pure Nothing
    Just l  -> pure $ decodeStrict l

sendJson :: (MonadIO m, ToJSON a) => Socket -> a -> m ()
sendJson skt x = send skt (LBS.toStrict $ encode x) >> send skt "\n"

-- | Receive until we obtain @\n@
recvLine :: MonadIO m => Socket -> m (Maybe ByteString)
recvLine skt = do
  mayFirst <- recv skt 1
  case mayFirst of
    Nothing    -> pure Nothing
    Just "\n"  -> pure $ Just ""
    Just first -> Just . (first <>) <$> go
  where
    go = do
      more <- recv skt 1
      case more of
        Nothing   -> pure mempty
        Just "\n" -> pure mempty
        Just c    -> (c <>) <$> go