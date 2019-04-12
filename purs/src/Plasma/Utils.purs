module Plasma.Utils where

import Prelude

import Control.Error.Util (note)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.ByteString as BS
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Traversable (for)
import Effect.Aff.Class (class MonadAff)
import Network.Ethereum.Core.BigNumber (embed)
import Network.Ethereum.Core.HexString (HexString, fromByteString, toByteString)
import Network.Ethereum.Web3 (Address, Web3)
import Network.Ethereum.Web3.Api (personal_sign)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Solidity.UInt (uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Vector (nilVector, (:<))
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Network.Ethereum.Web3.Types.Types (TransactionOptions)
import Partial.Unsafe (unsafeCrashWith)
import Plasma.Contracts.PlasmaMVP as PlasmaMVP
import Plasma.Routes as Routes
import Plasma.Types (Base64String(..), EthAddress(..), GetProofResp(..), Position(..), TendermintTransaction(..), UTXO(..))
import Servant.Api.Types (QueryParams(..), Required(..))
import Servant.Client.Request (AjaxError, ClientEnv)

makeConfirmationSignatureWithNode
  :: { signer :: Address
     , password :: Maybe String
     }
  -> UTXO
  -> Web3 Base64String
makeConfirmationSignatureWithNode {signer, password} (UTXO {confirmationHash}) = do
  let confirmationHashHex = fromByteString <<< un Base64String $ confirmationHash
  signatureHex <- personal_sign confirmationHashHex signer password
  pure <<< Base64String <<<  toByteString $ signatureHex

validateExitLengths
  :: { txBytes :: Base64String
     , proof :: Base64String
     , confirmationSignatures :: Array Base64String
     }
  -> Either String { txBytes :: Base64String
                   , proof :: Base64String
                   , confirmationSignatures :: Array Base64String
                   }
validateExitLengths {txBytes, proof, confirmationSignatures} = do
    txBytes' <- txBytes `validateLength` 811 `flip note` ("Invalid txBytes length, should be 811: " <> show txBytes)
    proof' <- proof `validateLengthModulo` 32 `flip note` ("Invalid proof length, should by multiple of 32: " <> show proof)
    sigs' <- for confirmationSignatures $ \sig ->
      sig `validateLengthModulo` 65 `flip note` ("Invalid signature length, should be 65 bytes: " <> show sig)
    pure { txBytes: txBytes'
         , proof: proof'
         , confirmationSignatures: sigs'
         }
  where
    validateLength a@(Base64String bs) n =
      if BS.length bs == n then Just a else Nothing
    validateLengthModulo a@(Base64String bs) n =
      if BS.length bs `mod` n == 0 then Just a else Nothing

exitUTXO
  :: forall m.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => TransactionOptions MinorUnit
  -> { utxo :: UTXO
     , ownerEth :: EthAddress
     , ownerPassword :: Maybe String
     , fee :: Int
     }
  -> m (Web3 HexString)
exitUTXO txOpts {utxo: utxo@(UTXO u), ownerEth, ownerPassword, fee} = do
  GetProofResp {proof: mProof, transaction} <- Routes.getProof $ QueryParams { ownerAddress: Required ownerEth
                                                                             , position: Required u.position
                                                                             }
  let coerceInt n = case uIntNFromBigNumber s256 (embed n) of
                      Nothing -> unsafeCrashWith "Int failed to be UINT"
                      Just a -> a
      txPos = case u.position of
        Position p -> coerceInt p.blockNumber :< coerceInt p.transactionIndex :< coerceInt p.outputIndex :< nilVector
      committedFee = coerceInt fee
      proof = case mProof of
        Nothing -> mempty
        Just (Base64String p) -> p
      txBytes = un Base64String (un TendermintTransaction transaction).tx
      EthAddress owner = ownerEth
  pure $ do
    Base64String confirmSignatures <- makeConfirmationSignatureWithNode {signer: owner, password: ownerPassword} utxo
    PlasmaMVP.startTransactionExit txOpts { txPos
                                          , txBytes
                                          , proof
                                          , confirmSignatures
                                          , committedFee
                                          }
