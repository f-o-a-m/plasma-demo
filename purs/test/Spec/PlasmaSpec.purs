module Spec.PlasmaSpec (plasmaSpec) where

import Prelude

import Chanterelle.Test (assertWeb3)
import Contracts.PlasmaMVP as PlasmaMVP
import Data.Either (Either(..))
import Data.Lens ((?~))
import Network.Ethereum.Web3 (Value, Wei, _from, _gas, _to, _value, defaultTransactionOptions, embed, mkValue)
import Spec.Config (PlasmaSpecConfig)
import Spec.Utils (unsafeMkUInt256, takeEventOrFail)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

plasmaSpec
  :: PlasmaSpecConfig
  -> Spec Unit
plasmaSpec {plasmaAddress, provider, users} = do
  describe "Plasma Root Contract" $
    it "can deposit some ETH into the rootchain contract" $ do
      let depositAmount = embed 1000
          txOpts = defaultTransactionOptions # _from ?~ users.bob
                                             # _to ?~ plasmaAddress
                                             # _gas  ?~ embed 9999999
                                             # _value ?~ (mkValue depositAmount :: Value Wei)
      eRes <- assertWeb3 provider <<< takeEventOrFail (Proxy :: Proxy PlasmaMVP.Deposit) provider plasmaAddress $
        PlasmaMVP.deposit txOpts { owner: users.bob
                                 }
      case eRes of
        Left txHash -> fail ("Failed to submit deposit: " <> show txHash)
        Right (PlasmaMVP.Deposit ev) -> do
          ev.depositor `shouldEqual` users.bob
          ev.amount `shouldEqual` unsafeMkUInt256 depositAmount
