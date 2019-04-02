module Spec.Plasma.PlasmaSpec (plasmaSpec) where

import Prelude

import Chanterelle.Test (assertWeb3)
import Contracts.PlasmaMVP as PlasmaMVP
import Data.Either (Either(..))
import Data.Lens ((?~))
import Network.Ethereum.Web3 (Value, Wei, _from, _gas, _to, _value, defaultTransactionOptions, embed, mkValue)
import Plasma.Routes as Routes
import Servant.Client.Request (assertRequest)
import Spec.Config (PlasmaSpecConfig)
import Spec.Plasma.Utils (takeEventOrFail, unsafeMkUInt256)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)
import Type.Proxy (Proxy(..))

plasmaSpec :: PlasmaSpecConfig -> Spec Unit
plasmaSpec cfg = do
  nodeHealthSpec cfg
  depositSpec cfg

nodeHealthSpec
  :: PlasmaSpecConfig
  -> Spec Unit
nodeHealthSpec {clientEnv} = do
  describe "Plasma Node Health" $
    it "can call the /health endoint on the plasma node" $ do
      health <- assertRequest clientEnv Routes.getHealth
      health `shouldEqual` "healthy"

depositSpec
  :: PlasmaSpecConfig
  -> Spec Unit
depositSpec {plasmaAddress, provider, users} = do
  let
    defaultPlasmaTxOptions = defaultTransactionOptions # _to ?~ plasmaAddress
                                                       # _gas  ?~ embed 8000000
  describe "Plasma Root Contract" $
    it "can deposit some ETH into the rootchain contract" $ do
      let depositAmount = embed 1000
          txOpts = defaultPlasmaTxOptions # _from ?~ users.bob
                                          # _value ?~ (mkValue depositAmount :: Value Wei)
      eRes <- assertWeb3 provider <<< takeEventOrFail (Proxy :: Proxy PlasmaMVP.Deposit) provider plasmaAddress $
        PlasmaMVP.deposit txOpts { owner: users.bob
                                 }
      case eRes of
        Left txHash -> fail ("Failed to submit deposit: " <> show txHash)
        Right (PlasmaMVP.Deposit ev) -> do
          ev.depositor `shouldEqual` users.bob
          ev.amount `shouldEqual` unsafeMkUInt256 depositAmount
