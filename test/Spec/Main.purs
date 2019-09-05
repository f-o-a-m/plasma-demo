module Spec.Main where

import Prelude

import Chanterelle.Internal.Utils.Web3 (pollTransactionReceipt)
import Chanterelle.Test (assertWeb3)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.Newtype (un)
import Data.String (Pattern(..), stripPrefix)
import Data.Time.Duration (Minutes(..), fromDuration)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (httpProvider)
import Node.Process as NP
import Plasma.Config.TOML (discoverPlasmaContractAddress)
import Spec.Config (PlasmaSpecConfig, getFinalizedPeriod, mkUsers)
import Spec.Plasma.PlasmaSpec (plasmaSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner as Runner
import Unsafe.Coerce (unsafeCoerce)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = launchAff_ $ do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  plasmaUrl <- liftEffect $ fromMaybe "http://localhost:1317" <$> NP.lookupEnv "PLASMA_URL"
  provider <- liftEffect $ httpProvider nodeUrl
  users <- mkUsers provider
  --  deployResults <- buildTestConfig nodeUrl 60 Deploy.deploy'
  plasmaAddress <- discoverPlasmaContractAddress
  --  mTx <- assertWeb3 provider $ transferOperatorFromMainAccount {plasmaAddress, account0: users.account0}
  --  case mTx of
  --    Nothing -> pure unit
  --    Just txHash -> void $ do
  --      C.log "Waiting to change operators... "
  --      pollTransactionReceipt txHash provider
  finalizedPeriod <- liftEffect getFinalizedPeriod
  let plasmaConfig :: PlasmaSpecConfig
      plasmaConfig = { plasmaAddress: plasmaAddress
                     , clientEnv: { protocol: "http"
                                  , baseURL: (unsafePartial $ fromJust $ stripPrefix (Pattern "http:") plasmaUrl) <> "/"
                                  }
                     , provider
                     , users
                     , finalizedPeriod
                     }
  do
    C.log "Running PlasmaSpec with config:"
    C.log (unsafeCoerce plasmaConfig)
  un Identity $ Runner.runSpecT Runner.defaultConfig {timeout = Just (fromDuration $ Minutes 6.0)} [consoleReporter] do
    plasmaSpec plasmaConfig
