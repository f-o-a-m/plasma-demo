module Spec.Plasma.PlasmaSpec (plasmaSpec) where

import Prelude

import Chanterelle.Test (assertWeb3)
import Data.Array (filter, head)
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Foldable (length)
import Data.Lens ((?~), (.~))
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class.Console as C
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Network.Ethereum.Core.HexString (fromByteString, hexLength, toByteString)
import Network.Ethereum.Core.Keccak256 (keccak256)
import Network.Ethereum.Core.Signatures as Sig
import Network.Ethereum.Web3 (class KnownSize, Address, Change(..), HexString, TransactionOptions, UIntN, Value, Wei, _from, _gas, _to, _value, defaultTransactionOptions, embed, mkValue, unUIntN)
import Network.Ethereum.Web3.Api (personal_sign)
import Network.Ethereum.Web3.Solidity.Size (kind DigitList)
import Network.Ethereum.Web3.Solidity.Sizes (S256)
import Network.Ethereum.Web3.Types (ETHER, NoPay)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
import Plasma.Contracts.PlasmaMVP as PlasmaMVP
import Plasma.Routes as Routes
import Plasma.Types (EthAddress(..), EthSignature(..), Input(..), Output(..), Position(..), PostDepositBody(..), Transaction(..), UTXO(..), emptyInput, emptyOutput, inputSignature, positionDepositNonce, removeEthereumSignatureShift, signatureFromByteString, transactionInput0, zeroPosition, zeroSignature)
import Plasma.Utils as Utils
import Servant.Api.Types (Captures(..), QueryParams(..), Required(..))
import Servant.Client.Request (assertRequest)
import Spec.Config (PlasmaSpecConfig)
import Spec.Plasma.Utils (defaultPassword, takeEventOrFail, unsafeMkUInt256, waitForBlocks)
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
      deposit users.bob cfg depositEth >>= case _ of
        Left txHash -> fail ("Failed to submit deposit XX: " <> show txHash)
        Right (Tuple (Change change) (PlasmaMVP.Deposit ev)) -> do
          C.log ("Desposit submitted succcessfully, txHash: " <> show change.transactionHash)
          ev.depositor `shouldEqual` users.bob
          ev.amount `shouldEqual` unsafeMkUInt256 depositAmount
          assertWeb3 provider $ waitForBlocks finalizedPeriod

          txHash <- includeDeposit users.bob cfg ev.depositNonce
          bobsUTXOs <- assertRequest clientEnv $ Routes.getUTXOs (Captures {owner: EthAddress users.bob})
          let utxo = head $ flip filter bobsUTXOs \(UTXO u) ->
                (un Position u.position).depositNonce == unsafeUIntNToInt ev.depositNonce
          utxo `shouldSatisfy` isJust

spendSpec :: PlasmaSpecConfig -> Spec Unit
spendSpec cfg@{plasmaAddress, users, provider, finalizedPeriod, clientEnv} = do
  describe "Plasma Root Contract" $
    it "can deposit ETH into the rootchain contract, transfer it to the sidechain and spend some ETH UTXO to another account" $ do
      let depositAmountEth = mkValue (embed 10000) :: Value Wei
          bob = users.bob
          alice = users.alice
      deposit bob cfg depositAmountEth >>= case _ of
        Left txHash -> fail ("Failed to submit deposit: " <> show txHash)
        Right (Tuple (Change change) (PlasmaMVP.Deposit ev)) -> do
          C.log ("Desposit submitted succcessfully, txHash: " <> show change.transactionHash)
          assertWeb3 provider $ waitForBlocks finalizedPeriod

          txHash <- includeDeposit users.bob cfg ev.depositNonce
          assertWeb3 provider $ waitForBlocks finalizedPeriod

          C.log ("Test that the UTXO exists on the plasma chain now and is unspent")
          let position =  zeroPosition # positionDepositNonce .~ unsafeUIntNToInt ev.depositNonce
          UTXO utxo <- assertRequest clientEnv $ Routes.getUTXO (QueryParams  { ownerAddress : Required $ EthAddress users.bob
                                                                              , position : Required position
                                                                              }
                                                                )
          utxo.spent `shouldEqual` false

          let confirmSignatures = [] -- leave it empty, as its the same as not setting `flagConfirmSigs0` or `flagConfirmSigs1` by using cli
              spendAmount = 10000
              input0 = Input
                { position
                , signature: zeroSignature
                , confirmSignatures
                }
              output0 = Output
                { owner: EthAddress alice
                , amount: spendAmount
                }
              transaction = Transaction
                { input0
                , input1: emptyInput
                , output0
                , output1: emptyOutput
                , fee: 0
                }
          C.log $ "Generate tx hash on server-side ... "
          transactionHash <- assertRequest clientEnv $ Routes.postTxHash transaction
          C.log $ "Signing transaction hash with eth node... "
          signatureHex <- assertWeb3 provider $ personal_sign transactionHash bob $ Just defaultPassword
          let signature@(EthSignature sig) = removeEthereumSignatureShift <<< signatureFromByteString <<< toByteString $ signatureHex
              messageBS = keccak256 <<< makeRidiculousEthereumMessage $ transactionHash
          -- sanity check to make sure the signature even has the possibility to be correct
          Sig.publicToAddress (Sig.recoverSender messageBS sig) `shouldEqual` bob
          let transaction' = transaction # transactionInput0 <<< inputSignature .~ signature
          C.log $ "Spending " <> show spendAmount <> " from " <> show bob <> " to " <> show alice
          _ <- assertRequest clientEnv $ Routes.postSpend transaction'
          alicesUTXOs <- assertRequest clientEnv $ Routes.getUTXOs (Captures {owner: EthAddress alice})
          -- TODO: DO this smarter if you want to continue to write parallel tests
          C.log "Making sure that alice has at least one unspent UTXO"
          length alicesUTXOs `shouldNotEqual` 0
          let alicesUTXO = unsafePartial fromJust $ head alicesUTXOs
              exitTxOpts = defaultPlasmaTxOptions # _from ?~ alice
                                                  # _value ?~ mkValue zero
                                                  # _to ?~ plasmaAddress
          exitTransaction <- assertRequest clientEnv $
               Utils.exitUTXO exitTxOpts { utxo: alicesUTXO
                                         , ownerEth: EthAddress alice
                                         , ownerPassword: Just defaultPassword
                                         , fee: 0
                                         }
          eExitRes <- assertWeb3 provider $
                takeEventOrFail (Proxy :: Proxy PlasmaMVP.StartedTransactionExit) provider plasmaAddress exitTransaction
          case eExitRes of
            Left exitTxHash -> fail ("Failed to submit startTransactionExit Tx: " <> show exitTxHash)
            Right (Tuple (Change exitChange) (PlasmaMVP.StartedTransactionExit exitEv)) -> do
              C.log ("TransactionExit submitted succcessfully, txHash: " <> show exitChange.transactionHash)
              C.logShow exitEv


defaultPlasmaTxOptions :: TransactionOptions NoPay
defaultPlasmaTxOptions = defaultTransactionOptions # _gas  ?~ embed 8000000

unsafeUIntNToInt :: forall n . KnownSize n => UIntN n -> Int
unsafeUIntNToInt = unsafeToInt <<< unUIntN

-- | Deposits ETH from an user (address) into root-chain contract
deposit
  :: Address
  -> PlasmaSpecConfig
  -> Value (MinorUnit ETHER)
  -> Aff (Either HexString (Tuple Change PlasmaMVP.Deposit))
deposit user {plasmaAddress, provider} amount = do
  let txOpts = defaultPlasmaTxOptions # _from ?~ user
                                      # _to ?~ plasmaAddress
                                      # _value ?~ amount
  C.log $ "Submitting deposit of " <> show amount <> " from " <> show user <> " to root chain contract"
  assertWeb3 provider $ takeEventOrFail (Proxy :: Proxy PlasmaMVP.Deposit) provider plasmaAddress $
              PlasmaMVP.deposit txOpts { owner: user
                                       }

-- | Includes deposit of an user (address) into the side-chain
includeDeposit
  :: Address
  -> PlasmaSpecConfig
  -> UIntN S256
  -> Aff String
includeDeposit user {provider, clientEnv} depositNonce = do
  let depositNonceInt = unsafeUIntNToInt depositNonce
      depositBody = PostDepositBody { ownerAddress: EthAddress user
                                    , depositNonce: show $ unsafeUIntNToInt depositNonce
                                    }
  C.log "Including deposit into side chain..."
  assertRequest clientEnv $ Routes.postIncludeDeposit depositBody


makeRidiculousEthereumMessage :: HexString -> HexString
makeRidiculousEthereumMessage s =
  let prefix = fromByteString <<< BS.toUTF8 $ "\EMEthereum Signed Message:\n" <> show (hexLength s `div` 2)
  in prefix <> s
