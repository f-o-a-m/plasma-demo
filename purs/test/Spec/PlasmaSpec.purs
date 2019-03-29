module Spec.PlasmaSpec (plasmaSpec) where

import Prelude

import Network.Ethereum.Web3 (defaultTransactionOptions)
import Spec.Config (PlasmaSpecConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

plasmaSpec :: PlasmaSpecConfig -> Spec Unit
plasmaSpec {plasmaAddress} = do
  describe "Plasma Root Contract" $
    it "can deposit some ETH into the rootchain contract" $ do
      -- let txOpts = defaultTransactionOptions # _from
      true `shouldEqual` true
