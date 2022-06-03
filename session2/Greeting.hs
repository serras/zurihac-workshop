{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language DeriveGeneric, DeriveAnyClass #-}
module Greeting where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString
import Data.Text
import GHC.Generics
import Network.Simple.TCP

data Request = WhoKnows deriving (Generic)
type Response = Text

greetingServer :: IO ()
greetingServer = serve "127.0.0.1" "8080" $ \(skt, adr) -> go skt
  where
    go :: Socket -> IO ()
    go skt = do
      -- read and parse request
      next <- recvLine skt
      -- send a response
      go skt

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