module Spec.Plasma.PlasmaSpec (plasmaSpec) where

import Prelude

import Chanterelle.Test (assertWeb3)
import Contracts.PlasmaMVP as PlasmaMVP
import Data.Array (filter, head)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (isJust)
import Data.Newtype (over2, un)
import Data.Tuple (Tuple(..))
import Effect.Class.Console as C
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Network.Ethereum.Web3 (BlockNumber(..), Change(..), Value, Wei, _from, _gas, _to, _value, defaultTransactionOptions, embed, mkValue, unUIntN)
import Plasma.Routes as Routes
import Plasma.Types (EthAddress(..), Position(..), PostDepositBody(..), UTXO(..))
import Servant.Api.Types (Captures(..))
import Servant.Client.Request (assertRequest)
import Spec.Config (PlasmaSpecConfig)
import Spec.Plasma.Utils (takeEventOrFail, unsafeMkUInt256, waitForBlock)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual, shouldSatisfy)
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
depositSpec {plasmaAddress, clientEnv, provider, users, finalizedPeriod} = do
  let
    defaultPlasmaTxOptions = defaultTransactionOptions # _to ?~ plasmaAddress
                                                       # _gas  ?~ embed 8000000
  describe "Plasma Root Contract" $
    it "can deposit some ETH into the rootchain contract, transfer it to the sidechain, and find the utxo" $ do
      let depositAmount = embed 1000
          txOpts = defaultPlasmaTxOptions # _from ?~ users.bob
                                          # _value ?~ (mkValue depositAmount :: Value Wei)
      C.log "Submitting deposit to root chain contract..."
      eRes <- assertWeb3 provider <<< takeEventOrFail (Proxy :: Proxy PlasmaMVP.Deposit) provider plasmaAddress $
        PlasmaMVP.deposit txOpts { owner: users.bob
                                 }
      case eRes of
        Left txHash -> fail ("Failed to submit deposit: " <> show txHash)
        Right (Tuple (Change change) (PlasmaMVP.Deposit ev)) -> do
          C.log ("Desposit submitted succcessfully, txHash: " <> show change.transactionHash)
          ev.depositor `shouldEqual` users.bob
          ev.amount `shouldEqual` unsafeMkUInt256 depositAmount
          let depositNonceInt = unsafeToInt <<< unUIntN $ ev.depositNonce
              depositBody = PostDepositBody { ownerAddress: EthAddress users.bob
                                            , depositNonce: show depositNonceInt
                                            }
              depositFinalizedBlock = over2 BlockNumber (+) change.blockNumber finalizedPeriod
          assertWeb3 provider $ waitForBlock depositFinalizedBlock
          txHash <- assertRequest clientEnv $ Routes.postIncludeDeposit depositBody
          bobsUTXOs <- assertRequest clientEnv $ Routes.getUTXOs (Captures {owner: EthAddress users.bob
                                                                           }
                                                                 )
          let utxo = head $ flip filter bobsUTXOs \(UTXO u) ->
                (un Position u.position).depositNonce == depositNonceInt
          utxo `shouldSatisfy` isJust
