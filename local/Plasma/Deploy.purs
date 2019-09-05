module Plasma.Deploy (deploy) where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (ContractConfig, NoArgs, DeployConfig(..), DeployM, noArgs, constructorNoArgs)
import Control.Monad.Reader (ask)
import Data.Lens ((?~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Validation.Semigroup (V, invalid)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber, embed)
import Network.Ethereum.Types (Address, HexString)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Network.Ethereum.Web3.Solidity (UIntN, uIntNFromBigNumber)
import Network.Ethereum.Web3 (_from, _gas, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)
--import Plasma.Contracts.PlasmaMVP as PlasmaMVP
import Plasma.Contracts.RootChain as RootChain
import Plasma.Contracts.ValidatorManagerContract as ValidatorManagerContract

validatorManagerContract :: ContractConfig NoArgs
validatorManagerContract =
  { filepath: "abis/ValidatorManagerContract.json"
  , name: "ValidatorManagerContract"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

type RootChain = (_vmc :: Address)

mkRootChain :: Record RootChain -> ContractConfig RootChain
mkRootChain { _vmc} =
    { filepath: "abis/RootChain.json"
    , name: "RootChain"
    , constructor: RootChain.constructor
    , unvalidatedArgs: pure { _vmc }
    }

--------------------------------------------------------------------------------
-- | PlasmaMVP
--------------------------------------------------------------------------------

-- plasmaTestConfig :: ContractConfig ( _exitDelay :: UIntN S256
--                                    , _depositChallengePeriod :: UIntN S256
--                                    , _nonDepositChallengePeriod :: UIntN S256
--                                    , _minExitBond :: UIntN S256
--                                    )
-- plasmaTestConfig =
--     { filepath: "abis/PlasmaMVP.json"
--     , name: "PlasmaMVP"
--     , constructor: PlasmaMVP.constructor
--     , unvalidatedArgs: plasmaArgs
--     }
--   where
--     plasmaArgs = ado
--       _exitDelay <- uIntNFromBigNumber s256 (embed 10) ?? "_exitDelay must be a valid UINT"
--       _depositChallengePeriod <- uIntNFromBigNumber s256 (embed 10) ?? "_depositChallengePeriod must be a valid UINT"
--       _nonDepositChallengePeriod <- uIntNFromBigNumber s256 (embed 10) ?? "_nonDepositChallengePeriod must be a valid UINT"
--       _minExitBond <- uIntNFromBigNumber s256 (embed 200000) ?? "_minExitBond must be a valid UINT"
--       in { _exitDelay
--          , _depositChallengePeriod
--          , _nonDepositChallengePeriod
--          , _minExitBond
--          }

type DeployResults =
  { plasmaAddress :: Address
  , plasmaDeployHash  :: HexString
  }


deploy :: DeployM Unit
deploy = void deploy'

deploy' :: DeployM DeployResults
deploy' = do
  deployCfg@(DeployConfig {primaryAccount, provider}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "8000000"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  vmc <- deployContract txOpts validatorManagerContract
  let rootConfig = mkRootChain { _vmc: vmc.deployAddress }
  plasma  <- deployContract txOpts rootConfig
  pure { plasmaAddress: plasma.deployAddress
       , plasmaDeployHash: plasma.deployHash
       }

--------------------------------------------------------------------------------

validateWithError :: forall a. Maybe a -> String -> V (Array String) a
validateWithError mres msg = case mres of
  Nothing -> invalid [msg]
  Just res -> pure res
infixl 9 validateWithError as ??

