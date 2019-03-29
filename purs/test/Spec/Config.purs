module Spec.Config (PlasmaSpecConfig) where

import Network.Ethereum.Web3 (Address, Provider)
import Servant.Client.Request (ClientEnv)

type PlasmaSpecConfig =
  { plasmaAddress :: Address
  , provider :: Provider
  , clientEnv :: ClientEnv
  , alice :: Address
  , bob :: Address
  }
