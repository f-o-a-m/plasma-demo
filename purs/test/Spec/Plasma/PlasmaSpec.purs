module Spec.Plasma.PlasmaSpec (plasmaSpec) where

import Prelude

import Chanterelle.Test (assertWeb3)
import Plasma.Contracts.PlasmaMVP as PlasmaMVP
import Data.Array (filter, head)
import Data.Either (Either(..))
import Data.Lens ((?~))
import Data.Maybe (isJust)
import Data.Newtype (over2, un)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class.Console as C
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Network.Ethereum.Web3 (Address, BlockNumber(..), Change(..), HexString, TransactionOptions, UIntN, Value, Wei, _from, _gas, _value, defaultTransactionOptions, embed, mkValue, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Network.Ethereum.Web3.Types (ETHER, NoPay)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
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
  spendSpec cfg

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
depositSpec cfg@{plasmaAddress, clientEnv, provider, users, finalizedPeriod} = do
  describe "Plasma Root Contract" $
    it "can deposit some ETH into the rootchain contract, transfer it to the sidechain, and find the utxo" $ do
      C.log $ "plasmaAddress: " <> show plasmaAddress
      let depositAmount = embed 1000
          depositEth = mkValue depositAmount :: Value Wei
      deposit users.bob cfg (mkValue depositAmount :: Value Wei) >>= case _ of
        Left txHash -> fail ("Failed to submit deposit: " <> show txHash)
        Right (Tuple (Change change) (PlasmaMVP.Deposit ev)) -> do
          C.log ("Desposit submitted succcessfully, txHash: " <> show change.transactionHash)
          ev.depositor `shouldEqual` users.bob
          ev.amount `shouldEqual` unsafeMkUInt256 depositAmount
          txHash <- includeDeposit users.bob cfg ev.depositNonce change.blockNumber
          bobsUTXOs <- assertRequest clientEnv $ Routes.getUTXOs (Captures {owner: EthAddress users.bob
                                                                                    }
                                                                  )
          let utxo = head $ flip filter bobsUTXOs \(UTXO u) ->
                (un Position u.position).depositNonce == unsafeDepositNonceToInt ev.depositNonce
          utxo `shouldSatisfy` isJust

spendSpec :: PlasmaSpecConfig -> Spec Unit 
spendSpec cfg@{plasmaAddress, users, provider, finalizedPeriod, clientEnv} = do
  describe "Plasma Root Contract" $
    it "can deposit ETH into the rootchain contract, transfer it to the sidechain and spend some ETH to another account" $ do
      let depositAmount = embed 1000
          depositEth = mkValue depositAmount :: Value Wei
          bob = users.bob
      deposit bob cfg depositEth >>= case _ of
        Left txHash -> fail ("Failed to submit deposit: " <> show txHash)
        Right (Tuple (Change change) (PlasmaMVP.Deposit ev)) -> do
          C.log ("Desposit submitted succcessfully, txHash: " <> show change.transactionHash)
          txHash <- includeDeposit users.bob cfg ev.depositNonce change.blockNumber
          -- TODO (sectore) Call `spend` endpoint to POST a `Plasma.Types.Transaction` including a signature
          -- Fail as long as we dont have everything implemented here
          "not" `shouldEqual` "ready"


defaultPlasmaTxOptions :: TransactionOptions NoPay
defaultPlasmaTxOptions = defaultTransactionOptions # _gas  ?~ embed 8000000

unsafeDepositNonceToInt :: (UIntN S256) -> Int
unsafeDepositNonceToInt = unsafeToInt <<< unUIntN

-- | Deposits ETH from an user (address) into root-chain contract
deposit 
  :: Address 
  -> PlasmaSpecConfig
  -> Value (MinorUnit ETHER) 
  -> Aff (Either HexString (Tuple Change PlasmaMVP.Deposit))
deposit user {plasmaAddress, provider} amount = do
  let txOpts = defaultPlasmaTxOptions # _from ?~ user
                                      # _value ?~ amount
  C.log "Submitting deposit to root chain contract..."
  assertWeb3 provider <<< takeEventOrFail (Proxy :: Proxy PlasmaMVP.Deposit) provider plasmaAddress $
    PlasmaMVP.deposit txOpts { owner: user
                             }

-- | Includes deposit of an user (address) into the side-chain
includeDeposit 
  :: Address
  -> PlasmaSpecConfig
  -> UIntN S256
  -> BlockNumber 
  -> Aff String
includeDeposit user {provider, finalizedPeriod, clientEnv} depositNonce blockNumber = do 
  let depositNonceInt = unsafeDepositNonceToInt depositNonce
      depositBody = PostDepositBody { ownerAddress: EthAddress user
                                    , depositNonce: show $ unsafeDepositNonceToInt depositNonce
                                    }
      depositFinalizedBlock = over2 BlockNumber (+) blockNumber finalizedPeriod
  assertWeb3 provider $ waitForBlock depositFinalizedBlock
  C.log "Including deposit into side chain..."
  assertRequest clientEnv $ Routes.postIncludeDeposit depositBody
