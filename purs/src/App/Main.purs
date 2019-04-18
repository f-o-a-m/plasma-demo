module App.Main where

import Prelude

import App.Env as Env
import App.Types (runAppM)
import App.Wallet.Component as Wallet
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Partial.Unsafe (unsafeCrashWith)
import Web.DOM.ParentNode (QuerySelector(..))


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  el <- HA.selectElement $ QuerySelector "#app"
  case el of
    Nothing ->
      unsafeCrashWith "div#app has to be defined"
    Just el' -> do
      appConfig <- Env.make
      runUI (H.hoist (runAppM appConfig) Wallet.component) unit el'
