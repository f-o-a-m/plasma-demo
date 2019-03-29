module Spec.Config (PlasmaSpecConfig) where

import Network.Ethereum.Web3 (Address)

type PlasmaSpecConfig =
  { plasmaAddress :: Address
  , plasmaURL :: String
  }
