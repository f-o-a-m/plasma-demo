module Plasma.Config.Utils where

import Prelude

import Control.Error.Util (note)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect, liftEffect)
import Network.Ethereum.Core.HexString (mkHexString)
import Network.Ethereum.Core.Signatures (Address, PrivateKey, mkAddress, mkPrivateKey)
import Node.Process as NP
import Partial.Unsafe (unsafeCrashWith)

type FromStringReader a = String -> Either String a

requireEnvVar :: forall m a. MonadEffect m => String -> FromStringReader a -> m a
requireEnvVar var parse = liftEffect $ do
  mval <- NP.lookupEnv var
  case mval of
    Nothing -> unsafeCrashWith $ "Must specify " <> show var <> " env var"
    Just val -> case parse val of
      Left err -> unsafeCrashWith
        $ "Failed to parse env var: " <> show var
        <> ", containing: " <> show val
        <> ", with error: " <> err
      Right a -> pure a

asBoolean :: FromStringReader Boolean
asBoolean "true" = Right true
asBoolean "false" = Right false
asBoolean _ = Left "Invalid Boolean"

asInt :: FromStringReader Int
asInt = fromString >>> note "invalid Int"

asString :: FromStringReader String
asString = pure

asPrivateKey :: FromStringReader PrivateKey
asPrivateKey = (mkHexString >=> mkPrivateKey) >>> note "invalid PrivateKey"

asAddress :: FromStringReader Address
asAddress = (mkHexString >=> mkAddress) >>> note "invalid Address"
