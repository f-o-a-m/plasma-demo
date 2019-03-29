module Spec.Config where

import Prelude

import Chanterelle.Test (assertWeb3)
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Network.Ethereum.Web3 (Address, Provider)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Partial.Unsafe (unsafeCrashWith)
import Servant.Client.Request (ClientEnv)

type Users =
  { alice :: Address
  , bob :: Address
  }


mkUsers :: Provider -> Aff Users
mkUsers provider = do
  accounts <- assertWeb3 provider eth_getAccounts
  case mkUsers' accounts of
    Nothing -> (unsafeCrashWith "Accounts list has less than two elements")
    Just us -> pure us
  where
    mkUsers' accs = do
      bob <- accs !! 0
      alice <- accs !! 1
      pure $ { bob
             , alice
             }

type PlasmaSpecConfig =
  { plasmaAddress :: Address
  , provider :: Provider
  , clientEnv :: ClientEnv
  , users :: Users
  }
