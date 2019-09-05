module Spec.Config where

import Prelude

import Chanterelle.Test (assertWeb3)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Core.HexString (HexString)
import Network.Ethereum.Core.Signatures (privateToAddress)
import Network.Ethereum.Web3 (Address, BlockNumber(..), Provider, embed)
import Network.Ethereum.Web3.Api (eth_getAccounts)
import Network.Ethereum.Web3.Types.TokenUnit (NoPay)
import Network.Ethereum.Web3.Types.Types (ChainCursor(..), TransactionOptions, Web3, _from, _gas, _to, defaultTransactionOptions)
import Node.Process as NP
import Partial.Unsafe (unsafeCrashWith)
import Plasma.Config.Utils (asPrivateKey, requireEnvVar)
--import Plasma.Contracts.PlasmaMVP as PlasmaMVP
import Plasma.Contracts.RootChain as RootChain
import Servant.Client.Request (ClientEnv)

type Users =
  { alice :: Address
  , bob :: Address
  , account0 :: Address
  }

defaultPlasmaTxOptions :: TransactionOptions NoPay
defaultPlasmaTxOptions = defaultTransactionOptions # _gas  ?~ embed 8000000


-- transferOperatorFromMainAccount
--   :: { plasmaAddress :: Address
--      , account0 :: Address
--      }
--   -> Web3 (Maybe HexString)
-- transferOperatorFromMainAccount {plasmaAddress, account0} = do
--   operatorKey <- requireEnvVar "OPERATOR_PRIVATE_KEY" asPrivateKey
--   let operatorAddress = privateToAddress operatorKey
--       txOpts = defaultPlasmaTxOptions # _to ?~ plasmaAddress
--                                       # _from ?~ account0
--   eOperator <- PlasmaMVP.operator txOpts Latest
--   case eOperator of
--     Left err -> liftEffect $ throw "Error when switching to new operator."
--     Right currentOperator ->
--       if currentOperator == operatorAddress
--          then pure Nothing
--          else Just <$> PlasmaMVP.changeOperator txOpts {newOperator: operatorAddress}


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
             , account0: bob
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
