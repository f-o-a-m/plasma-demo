module Spec.Config where

import Prelude

import Chanterelle.Test (assertWeb3)
import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Web3 (Address, BlockNumber(..), Provider)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Node.Process as NP
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

getFinalizedPeriod :: Effect BlockNumber
getFinalizedPeriod = do
  mfp <- NP.lookupEnv "FINALIZED_PERIOD"
  case mfp of
    Nothing -> (unsafeCrashWith "Must specify FINALIZED_PERIOD env var")
    Just fp -> case BlockNumber <$> parseBigNumber decimal fp of
      Nothing -> (unsafeCrashWith "Invalid FINALIZED_PERIOD: must be BlockNumber")
      Just bn -> pure bn

type PlasmaSpecConfig =
  { plasmaAddress :: Address
  , provider :: Provider
  , clientEnv :: ClientEnv
  , users :: Users
  , finalizedPeriod :: BlockNumber
  }
