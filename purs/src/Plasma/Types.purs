module Plasma.Types where

import Prelude

import Control.Monad.Except (runExcept, withExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, un)
import Foreign (F)
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON, genericDecode, defaultOptions)
import Foreign.Generic.Types (Options)
import Network.Ethereum.Web3 (Address)
import Servant.Api.Types (class ToCapture)
import Servant.Client.Client (Decoder)

newtype Position =
  Position { blockNumber :: Int
           , transactionIndex :: Int
           , outputIndex :: Int
           , depositNonce :: Int
           }

derive instance genericPosition :: Generic Position _
instance decodePosition :: Decode Position where
  decode = genericDecode plasmaOptions

newtype UTXO =
  UTXO { inputKeys :: Array String
       , spenderKeys :: Array String
       , confirmationHash :: String
       , "MerkleHash" :: String
       , spent :: Boolean
       , position :: Position
       }

derive instance genericUTXO :: Generic UTXO _
instance decodeUTXO :: Decode UTXO where
  decode = genericDecode plasmaOptions

newtype EthAddress = EthAddress Address
derive instance newtypeEthAddress :: Newtype EthAddress _

instance captureEthAddress :: ToCapture EthAddress where
  toCapture = show <<< un EthAddress

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


ld :: Decoder String (Array UTXO)
ld = genericDecoder
