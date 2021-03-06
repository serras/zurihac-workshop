{-# language OverloadedStrings #-}
{-# language TypeApplications #-}
module DiceRoll where

import Control.Monad.IO.Class
import Data.ByteString
import Data.Serialize
import Data.Word
import Network.Simple.TCP
import System.Random

diceServer :: IO ()
diceServer = serve "127.0.0.1" "8080" $ \(skt, adr) -> go skt
  where
    go :: Socket -> IO ()
    go skt = do
      -- read and parse request
      mayBytes <- recv skt 8
      case mayBytes of
        Nothing -> pure ()
        Just bytes -> do
          let Right max = decode @Word64 bytes
          response <- encode <$> randomRIO (1, max)
          -- send a response
          send skt response
          go skt

diceClient :: Word64 -> IO Word64
diceClient n = _
