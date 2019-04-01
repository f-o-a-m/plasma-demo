module Deploy where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (ContractConfig, DeployConfig(..), DeployM, NoArgs, constructorNoArgs, noArgs)
import Control.Monad.Reader (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber)
import Network.Ethereum.Types (Address, HexString)
import Network.Ethereum.Web3 (_from, _gas, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)

--------------------------------------------------------------------------------
-- | PlasmaMVP
--------------------------------------------------------------------------------

plasmaTestConfig :: ContractConfig NoArgs
plasmaTestConfig =
  { filepath: "abis/PlasmaMVP.json"
  , name: "PlasmaMVP"
  , constructor: constructorNoArgs
  , unvalidatedArgs: noArgs
  }

type DeployResults =
  { plasmaAddress :: Address
  , plasmaDeployHash  :: HexString
  }

deployScript :: DeployM DeployResults
deployScript = do
  deployCfg@(DeployConfig {primaryAccount, provider}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber decimal "8000000"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  plasma  <- deployContract txOpts plasmaTestConfig
  pure { plasmaAddress: plasma.deployAddress
       , plasmaDeployHash: plasma.deployHash
       }
