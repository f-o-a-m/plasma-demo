module Spec.PlasmaSpec (plasmaSpec) where

import Prelude

import Spec.Config (PlasmaSpecConfig)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

plasmaSpec :: PlasmaSpecConfig -> Spec Unit
plasmaSpec _ = do
  describe "Plasma Root Contract" $
    it "should check a tautology" $ do
      true `shouldEqual` true
