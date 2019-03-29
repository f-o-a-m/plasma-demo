module Spec.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Time.Duration (Minutes(..), fromDuration)
import Deploy as Deploy
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Process as NP
import Spec.PlasmaSpec (plasmaSpec)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner as Runner

main :: Effect Unit
main = launchAff_  do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  deployResults <- buildTestConfig nodeUrl 60 Deploy.deployScript
  let plasmaConfig = { plasmaAddress: deployResults.plasmaAddress
                     , plasmaURL: "http://127.0.0.1:1317"
                     }
  un Identity $ Runner.runSpecT Runner.defaultConfig {timeout = Just (fromDuration $ Minutes 6.0)} [consoleReporter] do
    plasmaSpec plasmaConfig
