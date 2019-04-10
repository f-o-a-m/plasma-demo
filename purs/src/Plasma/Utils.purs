module Plasma.Utils where

import Prelude

import Control.Error.Util (note)
import Data.ByteString as BS
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Traversable (for)
import Network.Ethereum.Core.HexString (fromByteString, toByteString)
import Network.Ethereum.Web3 (Address, Web3)
import Network.Ethereum.Web3.Api (personal_sign)
import Plasma.Types (Base64String(..), UTXO(..))

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
