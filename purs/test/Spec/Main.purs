module Spec.Main where

import Prelude

--import Chanterelle.Test (buildTestConfig)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Time.Duration (Minutes(..), fromDuration)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Network.Ethereum.Web3 (httpProvider, mkAddress, mkHexString)
import Node.Process as NP
import Partial.Unsafe (unsafeCrashWith)
--import Plasma.Deploy as Deploy
import Spec.Config (PlasmaSpecConfig, mkUsers)
import Spec.Plasma.PlasmaSpec (plasmaSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner as Runner

main :: Effect Unit
main = launchAff_  do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  provider <- liftEffect $ httpProvider nodeUrl
  users <- mkUsers provider
  --deployResults <- buildTestConfig nodeUrl 60 Deploy.deploy
  plasmaAddressStr <- liftEffect (NP.lookupEnv "PLASMA_ADDRESS")
  let plasmaAddress = case plasmaAddressStr >>= mkHexString >>= mkAddress of
        Nothing -> unsafeCrashWith "Must provide PLASMA_ADDRESS env var."
        Just addr -> addr
  let plasmaConfig :: PlasmaSpecConfig
      plasmaConfig = { plasmaAddress: plasmaAddress
                     , clientEnv : { protocol: "http"
                                   , baseURL: "//127.0.0.1:1317/"
                                   }
                     , provider
                     , users
                     }
  un Identity $ Runner.runSpecT Runner.defaultConfig {timeout = Just (fromDuration $ Minutes 6.0)} [consoleReporter] do
    plasmaSpec plasmaConfig
