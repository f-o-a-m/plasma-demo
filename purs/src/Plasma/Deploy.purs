module Plasma.Deploy (main, deploy) where

import Prelude

import Chanterelle.Deploy (deployContract, runDeployM)
import Chanterelle.Internal.Types (ContractConfig, DeployConfig(..), DeployM, NoArgs, constructorNoArgs, noArgs)
import Control.Monad.Reader (ask)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Types (Address, HexString)
import Network.Ethereum.Web3 (_from, _gas, defaultTransactionOptions, httpProvider, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts, net_version)
import Node.Process as NP
import Partial.Unsafe (unsafeCrashWith, unsafePartial)

--------------------------------------------------------------------------------
-- | PlasmaMVP
--------------------------------------------------------------------------------

plasmaTestConfig :: ContractConfig NoArgs
plasmaTestConfig =
  { filepath: "abis/PlasmaMVP.json"
  , name: "PlasmaMVP"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

type DeployResults =
  { plasmaAddress :: Address
  , plasmaDeployHash  :: HexString
  }


deploy :: DeployM DeployResults
deploy = do
  deployCfg@(DeployConfig {primaryAccount, provider}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "8000000"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  plasma  <- deployContract txOpts plasmaTestConfig
  pure { plasmaAddress: plasma.deployAddress
       , plasmaDeployHash: plasma.deployHash
       }

main :: Effect Unit
main = launchAff_ do
  cfg <- makeDeployConfig
  void $ runDeployM deploy cfg

makeDeployConfig :: Aff DeployConfig
makeDeployConfig = do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  provider <- liftEffect $ httpProvider nodeUrl
  eethData <- runWeb3 provider do
    accounts <- eth_getAccounts
    networkId <- net_version
    pure $ {accounts, networkId}
  case eethData of
      Left e -> unsafeCrashWith ("Error building deploy config: " <> show e)
      Right ethData -> case ethData.accounts !! 0 of
        Nothing -> unsafeCrashWith ("Error building deploy config: No primary account")
        Just primaryAccount ->
          pure $ DeployConfig { primaryAccount
                              , networkId: ethData.networkId
                              , writeArtifacts: true
                              , timeout: Milliseconds (60.0 * 1000.0)
                              , provider
                              }
