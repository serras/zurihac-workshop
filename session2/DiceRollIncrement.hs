{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
{-# language DeriveGeneric, DeriveAnyClass #-}
module DiceRollIncrement where

import Control.Monad.IO.Class
import Data.ByteString
import Data.Serialize
import Data.Word
import GHC.Generics
import Network.Simple.TCP
import System.Random

data Request 
  = DiceRoll  { max    :: Word64 }
  | Increment { number :: Word64 }
  deriving (Generic, Serialize)

type Response = Word64

diceServer :: IO ()
diceServer = serve "127.0.0.1" "8080" $ \(skt, adr) -> go skt
  where
    go :: Socket -> IO ()
    go skt = do
      -- read and parse request
      mayBytes <- recv skt 9  -- 1 + 8 info
      case mayBytes of
        Nothing -> pure ()
        Just bytes -> do
          let Right req = decode @Request bytes
          response <- case req of
            DiceRoll max -> randomRIO (1, max)
            Increment n  -> pure (n + 1)
              -- send a response
          send skt (encode response)
          go skt

diceClient :: Word64 -> IO Word64
diceClient n = connect "127.0.0.1" "8080" $ \(skt, _) -> do
  send skt (encode $ DiceRoll n)
  Just bytes <- recv skt 8
  let Right result = decode @Word64 bytes
  pure result

incrementClient :: Word64 -> IO Word64
incrementClient n = connect "127.0.0.1" "8080" $ \(skt, _) -> do
  send skt (encode $ Increment n)
  Just bytes <- recv skt 8
  let Right result = decode @Word64 bytes
  pure result
