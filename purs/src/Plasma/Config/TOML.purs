module Plasma.Config.TOML
  ( PlasmaConfig
  , TimeInterval
  , writePlasmaConfig
  , makeConfigFromEnvironment
  , discoverPlasmaContractAddress
  ) where

import Prelude

import Chanterelle.Internal.Deploy (readDeployAddress)
import Chanterelle.Internal.Logging (logDeployError)
import Control.Error.Util (note)
import Control.Monad.Except (runExceptT)
import Data.Array ((:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as C
import Effect.Exception (throw)
import Heterogeneous.Folding (class FoldingWithIndex, hfoldlWithIndex)
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal)
import Network.Ethereum.Core.HexString (unHex)
import Network.Ethereum.Core.Signatures (PrivateKey, unPrivateKey, mkPrivateKey)
import Network.Ethereum.Web3 (Address, Provider, runWeb3, httpProvider, mkHexString, mkAddress)
import Network.Ethereum.Web3.Api (net_version)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (writeTextFile)
import Node.Process as NP
import Partial.Unsafe (unsafeCrashWith)

{-

EXAMPLE

# This is a TOML config file.
# For more information, see https://github.com/toml-lang/toml

##### ethereum config options #####
# Boolean specifying if this node is the operator of the plasma contract
is_operator = "true"

# Hex encoded private key
# Used to sign eth transactions interacting with the contract
ethereum_operator_privatekey = "ac4d28b45202507d3475761c1e9277d3efc5a8ae8cfd690d979083ce241dd6d8"

# Ethereum plasma contract address
ethereum_plasma_contract_address = "0x05df3e846693afefce3dd9b6701787ad1d6cd9b1"

# Plasma block commitment rate. i.e 1m30s, 1m, 1h, etc.
plasma_block_commitment_rate = "2s"

# Node URL for eth client
ethereum_nodeurl = "http://localhost:8545"

# Number of Ethereum blocks until a submitted block header is considered final
ethereum_finality = "2"


-}

withQuotes :: String -> String
withQuotes s = "\""<> s <> "\""

newtype TimeInterval = TimeInterval Int

class TomlValue a where
  toTomlValue :: a -> String

instance tomlValueInt :: TomlValue Int where
  toTomlValue = show >>> withQuotes

instance tomlValueString :: TomlValue String where
  toTomlValue = show

instance tomlValueTimeInterval :: TomlValue TimeInterval where
  toTomlValue (TimeInterval n) = withQuotes $ show n <> "s"

instance tomlValuePrivateKey :: TomlValue PrivateKey where
  toTomlValue = unPrivateKey >>> unHex >>> withQuotes

instance tomlValueAddress :: TomlValue Address where
  toTomlValue = show >>> withQuotes

instance tomlValueBool :: TomlValue Boolean where
  toTomlValue = show >>> withQuotes

instance tomlValueMaybe :: TomlValue a => TomlValue (Maybe a) where
  toTomlValue = maybe (toTomlValue "") toTomlValue

type PlasmaConfig =
  { is_operator :: Boolean
  , ethereum_operator_privatekey :: Maybe PrivateKey
  , ethereum_plasma_contract_address :: Address
  , plasma_block_commitment_rate :: TimeInterval
  , ethereum_nodeurl :: String
  , ethereum_finality :: Int
  }

--------------------------------------------------------------------------------
-- Generate toml file
--------------------------------------------------------------------------------

type TomlFile = Array (Tuple String String)

data TomlEntry = TomlEntry

instance tomlEntry :: (TomlValue a, IsSymbol sym) => FoldingWithIndex TomlEntry (SProxy sym) (Array (Tuple String String)) a (Array (Tuple String String)) where
  foldingWithIndex TomlEntry prop acc a = Tuple (reflectSymbol prop) (toTomlValue a) : acc

-- | Using the toTomlValue instances, fold over the PlasmaConfig record to build the toml file as a string
templateTomlFile :: PlasmaConfig -> String
templateTomlFile = joinWith "\n" <<< map (\(Tuple k v) -> k <> " = " <> v) <<< toTomlFile
  where
    toTomlFile :: PlasmaConfig -> TomlFile
    toTomlFile = hfoldlWithIndex TomlEntry ([] :: TomlFile)

-- | read a bunch env vars and dynamically try to figure out where the plasma contract address is coming from.
makeConfigFromEnvironment :: Aff PlasmaConfig
makeConfigFromEnvironment = do
  isOperator <- requireEnvVar "IS_OPERATOR" asBoolean
  operatorKey <- if isOperator
       then Just <$> requireEnvVar "OPERATOR_PRIVATE_KEY" asPrivateKey
       else pure Nothing
  commitmentRate <- requireEnvVar "COMMITMENT_RATE" asInt
  nodeURL <- requireEnvVar "NODE_URL" asString
  finality <- requireEnvVar "FINALIZED_PERIOD" asInt
  plasmaAddress <- discoverPlasmaContractAddress
  pure { is_operator: isOperator
       , ethereum_operator_privatekey: operatorKey
       , ethereum_plasma_contract_address: plasmaAddress
       , plasma_block_commitment_rate: TimeInterval commitmentRate
       , ethereum_nodeurl: nodeURL
       , ethereum_finality: finality
       }

-- | write the plasma config to a file.
writePlasmaConfig :: PlasmaConfig -> Aff Unit
writePlasmaConfig cfg = do
  configDest <- requireEnvVar "PLASMA_CONFIG_DESTINATION" asString
  let content = templateTomlFile cfg
  C.log ("Writing plasma config to " <> configDest)
  writeTextFile UTF8 configDest content

--------------------------------------------------------------------------------
-- | ConfigUtils
--------------------------------------------------------------------------------

data AddressSource =
    FromChanterelleArtifactFile Provider String
  | AddressLiteral Address


discoverPlasmaContractAddress :: Aff Address
discoverPlasmaContractAddress = do
  mAddr <- liftEffect (NP.lookupEnv "PLASMA_ADDRESS")
  case mAddr of
    Nothing -> do
      C.log "PLASMA_ADDRESS env var not found, trying to read from file..."
      fp <- requireEnvVar "PLASMA_ARTIFACT" asString
      provider <- requireEnvVar "NODE_URL" asString >>= liftEffect <<< httpProvider
      getPlasmaContractAddress (FromChanterelleArtifactFile provider fp)
    Just addr -> case mkHexString addr >>= mkAddress of
      Nothing -> liftEffect $ throw ("Error parsing PLASMA_ADDRESS: " <> show addr)
      Just addr' -> getPlasmaContractAddress (AddressLiteral addr')

getPlasmaContractAddress :: AddressSource -> Aff Address
getPlasmaContractAddress (AddressLiteral addr) = do
  C.log "Plasma Contract Address given as literal"
  pure addr
getPlasmaContractAddress (FromChanterelleArtifactFile provider filepath) = do
  C.log $ "Taking Plasma Contract Address from file: " <> filepath
  enId <- runWeb3 provider $ net_version
  case lmap show enId >>= \nid -> note ("Couldn't parse network version: " <> nid) $ parseBigNumber decimal nid of
    Left e -> liftEffect $ throw e
    Right nId -> do
      eRes <- runExceptT $ readDeployAddress filepath nId
      case eRes of
        Left e -> do
          logDeployError e
          liftEffect $ throw "error"
        Right res -> pure res


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
