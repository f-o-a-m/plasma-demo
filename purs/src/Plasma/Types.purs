module Plasma.Types where

import Prelude

import Control.Monad.Except (runExcept, withExcept)
import Data.Argonaut (jsonParser)
import Data.Argonaut as A
import Data.ByteString as BS
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Lens', (^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record as LR
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, un)
import Data.String (joinWith)
import Data.Symbol (SProxy(..))
import Foreign (F, ForeignError(..), fail)
import Foreign.Class (class Decode, class Encode, decode, encode)
import Foreign.Generic (decodeJSON, encodeJSON, genericDecode, genericEncode, defaultOptions)
import Foreign.Generic.Types (Options)
import Network.Ethereum.Core.BigNumber (decimal, parseBigNumber, toString)
import Network.Ethereum.Core.HexString (fromByteString, toByteString, nullWord)
import Network.Ethereum.Core.RLP (RLPObject(..))
import Network.Ethereum.Core.Signatures as Sig
import Network.Ethereum.Web3 (Address, BigNumber, mkAddress, mkHexString)
import Network.HTTP.Affjax.Request as Request
import Node.Buffer.Unsafe (slice)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Servant.Api.Types (class EncodeQueryParam, class ToCapture)
import Servant.Client.Client (Decoder, Encoder)
import Type.Quotient (mkQuotient, runQuotient)


newtype EthAddress = EthAddress Address
derive instance genericEthAddress :: Generic EthAddress _
derive instance newtypeEthAddress :: Newtype EthAddress _

instance encodeEthAddress :: Encode EthAddress where
  encode = genericEncode plasmaOptions

instance captureEthAddress :: ToCapture EthAddress where
  toCapture = show <<< un EthAddress

instance encodeQueryParamEthAddress :: EncodeQueryParam EthAddress where
  encodeQueryParam = un EthAddress >>> show

--------------------------------------------------------------------------------

newtype Base64String = Base64String BS.ByteString

derive instance genericBase64String :: Generic Base64String _
derive instance newtypeBase64String :: Newtype Base64String _

instance showBase64String :: Show Base64String where
  show = flip BS.toString BS.Base64 <<< un Base64String


instance eqBase64String :: Eq Base64String where
  eq = genericEq

instance decodeBase64String :: Decode Base64String where
  decode x = do
    s <- decode x
    case BS.fromString s BS.Base64 of
      Nothing -> fail (ForeignError $ "Failed to parse as Base64String: " <> s)
      Just bs -> pure $ Base64String bs

instance encodeBase64String :: Encode Base64String where
  encode = encode <<< flip BS.toString BS.Base64 <<< un Base64String

emptyBase64String :: Base64String
emptyBase64String = Base64String BS.empty

--------------------------------------------------------------------------------

newtype IntString = IntString BigNumber

derive instance genericIntString :: Generic IntString _
derive instance newtypeIntString :: Newtype IntString _

derive newtype instance showIntString :: Show IntString

instance decodeIntString :: Decode IntString where
  decode x = do
    s <- decode x
    case parseBigNumber decimal s of
      Nothing -> fail (ForeignError $ "Failed to parse as IntString: " <> s)
      Just is -> pure $ IntString is

instance encodeIntString :: Encode IntString where
  encode = encode <<< toString decimal <<< un IntString

--------------------------------------------------------------------------------

newtype Position =
  Position { blockNumber :: Int
           , transactionIndex :: Int
           , outputIndex :: Int
           , depositNonce :: Int
           }

derive instance genericPosition :: Generic Position _
derive instance newtypePosition :: Newtype Position _
instance decodePosition :: Decode Position where
  decode = genericDecode plasmaOptions

instance showPosition :: Show Position where
  show = genericShow

instance eqPosition :: Eq Position where
  eq = genericEq

instance encodePosition :: Encode Position where
  encode = genericEncode plasmaOptions

instance encodeQueryParamPosition :: EncodeQueryParam Position where
  encodeQueryParam (Position p) =
    let indexes = map show [ p.blockNumber
                           , p.transactionIndex
                           , p.outputIndex
                           , p.depositNonce
                           ]
    in "(" <> joinWith "." indexes <> ")"

nullPosition :: Position
nullPosition = Position
  { blockNumber: 0
  , transactionIndex: 0
  , outputIndex: 0
  , depositNonce: 0
  }

positionBlockNumber :: Lens' Position Int
positionBlockNumber = _Newtype <<< LR.prop (SProxy :: SProxy "blockNumber")

positionTransactionIndex :: Lens' Position Int
positionTransactionIndex = _Newtype <<< LR.prop (SProxy :: SProxy "transactionIndex")

positionOutputIndex :: Lens' Position Int
positionOutputIndex = _Newtype <<< LR.prop (SProxy :: SProxy "outputIndex")

positionDepositNonce :: Lens' Position Int
positionDepositNonce = _Newtype <<< LR.prop (SProxy :: SProxy "depositNonce")

--------------------------------------------------------------------------------

newtype TendermintTransaction =
  TendermintTransaction { hash :: Base64String
                        , height :: IntString
                        , index :: Int
                        , tx :: Base64String
                        , proof :: Proof
                        }

instance showGetTendermintTransaction :: Show TendermintTransaction where
  show = genericShow

derive instance genericTendermintTransaction :: Generic TendermintTransaction _
derive instance newtypeTendermintTransaction :: Newtype TendermintTransaction _
instance decodeTendermintTransaction :: Decode TendermintTransaction where
  decode = genericDecode plasmaOptions

--------------------------------------------------------------------------------

newtype Proof =
  Proof { "RootHash" :: Base64String
        , "Data" :: Base64String
        , "Proof" :: { total :: IntString
                     , index :: IntString
                     , leaf_hash :: Base64String
                     , aunts :: Maybe (Array String)
                     }
        }

derive instance genericProof :: Generic Proof _
derive instance newtypeProof :: Newtype Proof _
instance decodeProof :: Decode Proof where
  decode = genericDecode plasmaOptions

instance showGetProof :: Show Proof where
  show = genericShow

--------------------------------------------------------------------------------

newtype GetProofResp =
  GetProofResp { transaction :: Transaction
               , proof :: Maybe Base64String
               }

derive instance genericGetProofResp :: Generic GetProofResp _
derive instance newtypeGetProofResp :: Newtype GetProofResp _
instance decodeGetProofResp :: Decode GetProofResp where
  decode = genericDecode plasmaOptions

instance showGetProofResp :: Show GetProofResp where
  show = genericShow

--------------------------------------------------------------------------------

newtype UTXO =
  UTXO { inputKeys :: Array Base64String
       , spenderKeys :: Array Base64String
       , confirmationHash :: Base64String
       , "MerkleHash" :: Base64String
       , spent :: Boolean
       , position :: Position
       }

derive instance genericUTXO :: Generic UTXO _

instance showUTXO :: Show UTXO where
  show = genericShow

instance eqUTXO :: Eq UTXO where
  eq = genericEq

instance decodeUTXO :: Decode UTXO where
  decode = genericDecode plasmaOptions

--------------------------------------------------------------------------------

newtype Input =
  Input { position :: Position
        , signature :: EthSignature
        , confirmSignatures :: Array EthSignature
        }

derive instance newtypeInput :: Newtype Input _
derive instance genericInput :: Generic Input _

instance showInput :: Show Input where
  show = genericShow

instance decodeInput :: Decode Input where
  decode = genericDecode plasmaOptions

instance encodeInput :: Encode Input where
  encode = genericEncode plasmaOptions

emptyInput :: Input
emptyInput = Input
  { position: nullPosition
  , signature: zeroSignature
  , confirmSignatures: []
  }

inputPosition :: Lens' Input Position
inputPosition = _Newtype <<< LR.prop (SProxy :: SProxy "position")

inputSignature :: Lens' Input EthSignature
inputSignature = _Newtype <<< LR.prop (SProxy :: SProxy "signature")

inputConfirmSignatures :: Lens' Input (Array EthSignature)
inputConfirmSignatures = _Newtype <<< LR.prop (SProxy :: SProxy "confirmSignatures")

--------------------------------------------------------------------------------

newtype Output =
  Output { owner :: Address
         , amount :: Int -- FIXME(sectore) It should be `BigNumber`, but `Go` deserialization seems to have an issue with it
         }

derive instance newtypeOutput :: Newtype Output _
derive instance genericOutput :: Generic Output _

instance showOutput :: Show Output where
  show = genericShow

instance decodeOutput :: Decode Output where
  decode = genericDecode plasmaOptions

instance encodeOutput :: Encode Output where
  encode = genericEncode plasmaOptions

emptyOutput :: Output
emptyOutput = Output
  { owner: zeroAddress
  , amount: 0
  }

outputOwner :: Lens' Output Address
outputOwner = _Newtype <<< LR.prop (SProxy :: SProxy "owner")

outputAmount :: Lens' Output Int
outputAmount = _Newtype <<< LR.prop (SProxy :: SProxy "amount")

--------------------------------------------------------------------------------

newtype Transaction =
  Transaction { input0 :: Input
              , input1 :: Input
              , output0 :: Output
              , output1 :: Output
              , fee :: Int -- FIXME(sectore) It should be `BigNumber`, but `Go` deserialization seems to have an issue with it
              }

derive instance newtypeTransaction :: Newtype Transaction _
derive instance genericTransaction :: Generic Transaction _

instance showTransaction :: Show Transaction where
  show = genericShow

instance decodeTransaction :: Decode Transaction where
  decode = genericDecode plasmaOptions

instance encodeTransaction :: Encode Transaction where
  encode = genericEncode plasmaOptions

transactionInput0 :: Lens' Transaction Input
transactionInput0 = _Newtype <<< LR.prop (SProxy :: SProxy "input0")

transactionInput1 :: Lens' Transaction Input
transactionInput1 = _Newtype <<< LR.prop (SProxy :: SProxy "input1")

makeTransactionRLP :: Transaction -> RLPObject
makeTransactionRLP (Transaction tx) = RLPArray
  [ RLPInt $ tx.input0 ^. (inputPosition <<< positionBlockNumber)       -- BlkNum0           [32]byte
  , RLPInt $ tx.input0 ^. (inputPosition <<< positionTransactionIndex)  -- TxIndex0          [32]byte
  , RLPInt $ tx.input0 ^. (inputPosition <<< positionOutputIndex)       -- OIndex0           [32]byte
  , RLPInt $ tx.input0 ^. (inputPosition <<< positionDepositNonce)      -- DepositNonce0     [32]byte
  , makeConfirmSignaturesRLP $ tx.input0 ^. inputConfirmSignatures      -- Input0ConfirmSigs [130]byte
  , RLPInt $ tx.input1 ^. (inputPosition <<< positionBlockNumber)       -- BlkNum1           [32]byte
  , RLPInt $ tx.input1 ^. (inputPosition <<< positionTransactionIndex)  -- TxIndex1          [32]byte
  , RLPInt $ tx.input1 ^. (inputPosition <<< positionOutputIndex)       -- OIndex1           [32]byte
  , RLPInt $ tx.input1 ^. (inputPosition <<< positionDepositNonce)      -- DepositNonce1     [32]byte
  , makeConfirmSignaturesRLP $ tx.input1 ^. inputConfirmSignatures      -- Input1ConfirmSigs [130]byte
  , RLPAddress $ tx.output0 ^. outputOwner                              -- NewOwner0         common.Address
  , RLPInt $ tx.output0 ^. outputAmount                                 -- Amount0           [32]byte
  , RLPAddress $ tx.output1 ^. outputOwner                              -- NewOwner1         common.Address
  , RLPInt $ tx.output1 ^. outputAmount                                 -- Amount1           [32]byte
  , RLPInt $ tx.fee                                                     -- Fee               [32]byte
  ]

makeConfirmSignaturesRLP :: Array EthSignature -> RLPObject
makeConfirmSignaturesRLP signatures = RLPArray $ RLPByteString <<< signatureToByteString <$> signatures


--------------------------------------------------------------------------------

newtype PostDepositBody =
  PostDepositBody { ownerAddress :: EthAddress
                  , depositNonce :: String
                  }

derive instance genericPostDepositBody :: Generic PostDepositBody _

instance encodePostDepositBody :: Encode PostDepositBody where
  encode = genericEncode plasmaOptions

--------------------------------------------------------------------------------
-- Signature utils
--------------------------------------------------------------------------------

newtype EthSignature = EthSignature Sig.Signature

derive instance eqEthSignature :: Eq EthSignature
instance showEthSignature :: Show EthSignature where
  show (EthSignature sig) = show sig

instance encodeEthSignature :: Encode EthSignature where
  encode = encode <<< Base64String <<< signatureToByteString

zeroSignature :: EthSignature
zeroSignature = EthSignature $ Sig.Signature {r: nullWord, s: nullWord, v: 0}

signatureToByteString :: EthSignature -> BS.ByteString
signatureToByteString (EthSignature (Sig.Signature sig)) =
   toByteString sig.r <> toByteString sig.s <> BS.singleton (mkQuotient sig.v)

signatureFromByteString :: BS.ByteString -> EthSignature
signatureFromByteString bs =
  let bfr = BS.unsafeThaw bs
      r = fromByteString $ BS.unsafeFreeze $ slice 0 32 bfr
      s = fromByteString $ BS.unsafeFreeze $ slice 32 64 bfr
      v = runQuotient $ unsafePartial fromJust $ BS.last bs
  in EthSignature $ Sig.Signature {r,s,v}

instance decodeEthSignature :: Decode EthSignature where
  decode x = signatureFromByteString <<< un Base64String <$> decode x

removeEthereumSignatureShift :: EthSignature -> EthSignature
removeEthereumSignatureShift (EthSignature (Sig.Signature sig)) = EthSignature $ Sig.Signature sig {v = sig.v - 27}

addEthereumSignatureShift :: EthSignature -> EthSignature
addEthereumSignatureShift (EthSignature (Sig.Signature sig)) = EthSignature $ Sig.Signature sig {v = sig.v + 27}

--------------------------------------------------------------------------------

fEither :: F ~> Either String
fEither = runExcept <<<  withExcept show

plasmaOptions :: Options
plasmaOptions = defaultOptions { unwrapSingleConstructors = true }

genericDecoder
  :: forall a.
     Decode a
  => Decoder String a
genericDecoder =
  { parse: Right
  , decode: fEither <<< decodeJSON
  }

genericEncoder
  :: forall a.
     Encode a
  => Encoder a A.Json
genericEncoder =
  { encode: unsafeFromRight <<< jsonParser <<< encodeJSON
  , print: Request.Json
  }
  where
    unsafeFromRight ea = case ea of
      Right a -> a
      Left e -> unsafeCrashWith ("unsafeFromRight Invalid Json encoding: " <> e)

zeroAddress :: Address
zeroAddress =
  case mkHexString "0x0000000000000000000000000000000000000000" >>= mkAddress of
    Nothing -> unsafeCrashWith "Invalid 'zero' address: "
    Just a -> a
