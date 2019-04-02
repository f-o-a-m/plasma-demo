module Plasma.Types where

import Prelude

import Control.Monad.Except (runExcept, withExcept)
import Data.Argonaut (jsonParser)
import Data.Argonaut as A
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, un)
import Foreign (F)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (decodeJSON, encodeJSON, genericDecode, genericEncode, defaultOptions)
import Foreign.Generic.Types (Options)
import Network.Ethereum.Web3 (Address)
import Network.HTTP.Affjax.Request as Request
import Partial.Unsafe (unsafeCrashWith)
import Servant.Api.Types (class ToCapture)
import Servant.Client.Client (Decoder, Encoder)

newtype EthAddress = EthAddress Address
derive instance genericEthAddress :: Generic EthAddress _
derive instance newtypeEthAddress :: Newtype EthAddress _

instance encodeEthAddress :: Encode EthAddress where
  encode = genericEncode plasmaOptions

instance captureEthAddress :: ToCapture EthAddress where
  toCapture = show <<< un EthAddress

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

--------------------------------------------------------------------------------

newtype UTXO =
  UTXO { inputKeys :: Array String
       , spenderKeys :: Array String
       , confirmationHash :: String
       , "MerkleHash" :: String
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

newtype PostDepositBody =
  PostDepositBody { ownerAddress :: EthAddress
                  , depositNonce :: String
                  }

derive instance genericPostDepositBody :: Generic PostDepositBody _

instance encodePostDepositBody :: Encode PostDepositBody where
  encode = genericEncode plasmaOptions


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

