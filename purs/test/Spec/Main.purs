module Spec.Main where

import Prelude

import Chanterelle.Test (assertWeb3, buildTestConfig)
import Data.Array ((!!))
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.Time.Duration (Minutes(..), fromDuration)
import Deploy as Deploy
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (httpProvider)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Node.Process as NP
import Partial.Unsafe (unsafeCrashWith)
import Spec.Config (PlasmaSpecConfig)
import Spec.PlasmaSpec (plasmaSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner as Runner

main :: Effect Unit
main = launchAff_  do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  provider <- liftEffect $ httpProvider nodeUrl
  deployResults <- buildTestConfig nodeUrl 60 Deploy.deployScript
  users <- do
    accounts <- assertWeb3 provider eth_getAccounts
    maybe (unsafeCrashWith "Accounts list has less than two elements") pure do
      bob <- accounts !! 0
      alice <- accounts !! 1
      pure $ { bob
             , alice
             }
  let plasmaConfig :: PlasmaSpecConfig
      plasmaConfig = { plasmaAddress: deployResults.plasmaAddress
                     , clientEnv : { protocol: "http"
                                   , baseURL: "127.0.0.1:1317"
                                   }
                     , provider
                     , alice: users.alice
                     , bob: users.bob
                     }
  un Identity $ Runner.runSpecT Runner.defaultConfig {timeout = Just (fromDuration $ Minutes 6.0)} [consoleReporter] do
    plasmaSpec plasmaConfig
