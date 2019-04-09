module Plasma.Config.TOMLMain (main) where

import Prelude
import Plasma.Config.TOML (makeConfigFromEnvironment, writePlasmaConfig)
import Effect (Effect)
import Effect.Aff (launchAff_)

main :: Effect Unit
main = launchAff_ do
  cfg <- makeConfigFromEnvironment
  writePlasmaConfig cfg
