module Plasma.Config.TOML where

import Prelude

import Chanterelle.Internal.Deploy (readDeployAddress)
import Chanterelle.Internal.Logging (logDeployError)
import Control.Error.Util (note)
import Control.Monad.Except (runExceptT)
import Data.Array ((:))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
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
  toTomlValue = identity >>> withQuotes

instance tomlValueTimeInterval :: TomlValue TimeInterval where
  toTomlValue (TimeInterval n) = withQuotes $ toTomlValue n <> "s"

instance tomlValuePrivateKey :: TomlValue PrivateKey where
  toTomlValue = unPrivateKey >>> unHex >>> withQuotes

instance tomlValueAddress :: TomlValue Address where
  toTomlValue = show >>> withQuotes

instance tomlValueBool :: TomlValue Boolean where
  toTomlValue = show >>> withQuotes

type PlasmaConfig =
  { is_operator :: Boolean
  , ethereum_operator_private_key :: PrivateKey
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

templateTomlFile :: PlasmaConfig -> String
templateTomlFile = joinWith "\n" <<< map (\(Tuple k v) -> k <> " = " <> v) <<< toTomlFile
  where
    toTomlFile :: PlasmaConfig -> TomlFile
    toTomlFile = hfoldlWithIndex TomlEntry ([] :: TomlFile)

makeConfigFromEnvironment :: Aff PlasmaConfig
makeConfigFromEnvironment = do
  isOperator <- requireEnvVarBool "IS_OPERATOR"
  privateKey <- requireEnvVar "OPERATOR_PRIVATE_KEY" >>= readVarWith (\key -> note ("InvalidPrivateKey " <> key) (mkHexString key >>= mkPrivateKey))
  plasmaAddress <- discoverPlasmaContractAddress
  commitmentRate <- requireEnvVar "COMMITMENT_RATE" >>= readVarWith (\i -> note ("Invalid Int " <> i) (fromString i))
  nodeURL <- requireEnvVar "NODE_URL"
  finality <- requireEnvVar "COMMITMENT_RATE" >>= readVarWith (\i -> note ("Invalid Int " <> i) (fromString i))
  pure { is_operator: isOperator
       , ethereum_operator_private_key: privateKey
       , ethereum_plasma_contract_address: plasmaAddress
       , plasma_block_commitment_rate: TimeInterval commitmentRate
       , ethereum_nodeurl: nodeURL
       , ethereum_finality: finality
       }

writePlasmaConfig :: PlasmaConfig -> Aff Unit
writePlasmaConfig cfg = do
  configDest <- requireEnvVar "CONFIG_DESTINATION"
  let content = templateTomlFile cfg
  C.log ("Writing plasma config to " <> configDest)
  writeTextFile UTF8 configDest content

configMain :: Effect Unit
configMain = launchAff_ do
  cfg <- makeConfigFromEnvironment
  writePlasmaConfig cfg

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
      fp <- requireEnvVar "PLASMA_ARTIFACT"
      provider <- requireEnvVar "NODE_URL" >>= liftEffect <<< httpProvider
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


requireEnvVar :: forall m. MonadEffect m => String -> m String
requireEnvVar var = liftEffect $ do
  mval <- NP.lookupEnv var
  case mval of
    Nothing -> unsafeCrashWith ("Must specify " <> var <> " env var")
    Just val -> pure val

requireEnvVarBool :: forall m. MonadEffect m => String -> m Boolean
requireEnvVarBool var = liftEffect $ do
  mval <- NP.lookupEnv var
  case mval of
    Nothing -> unsafeCrashWith ("Must specify " <> var <> " env var")
    Just val ->
      if val == "true"
        then pure true
        else if val == "false"
                then pure false
                else unsafeCrashWith ("Invalid Boolean env var " <> val)

readVarWith :: forall a m . MonadEffect m => (String -> Either String a) -> String -> m a
readVarWith parser val = liftEffect $ case parser val of
  Left e -> throw $ "Error reading env var: " <> e
  Right a -> pure a
