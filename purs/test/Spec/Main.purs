module Spec.Main where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Data.Maybe (fromMaybe)
import Deploy as Deploy
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Process as NP

main :: Effect Unit
main = launchAff_  do
  nodeUrl <- liftEffect $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  testCfg <- buildTestConfig nodeUrl 60 Deploy.deployScript
  pure unit
