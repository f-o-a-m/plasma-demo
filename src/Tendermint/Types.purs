module Tendermint.Types where

import Prelude

import Control.Monad.Except (runExcept, withExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, un)
import Foreign (F)
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON, defaultOptions, genericDecode)
import Foreign.Generic.Types (Options)
import Network.Ethereum.Web3 (HexString)
import Plasma.Types (Base64String)
import Servant.Api.Types (class EncodeQueryParam)
import Servant.Client.Client (Decoder)

newtype TransactionHash = TransactionHash HexString

derive instance newtypeTransactionHash :: Newtype TransactionHash _

instance encodeQueryParamTransactionHash :: EncodeQueryParam TransactionHash where
  encodeQueryParam = un TransactionHash >>> show


-- NOTE: there might be a higher level Response :: * -> * type, Response a,
-- and this will just become Response TransactionResult.
newtype TransactionResultResponse =
  TransactionResultResponse { result ::
                                 { tx :: Base64String
                                 }
                            }

instance showGetTransactionResultResponse :: Show TransactionResultResponse where
  show = genericShow

derive instance genericTransactionResultResponse :: Generic TransactionResultResponse _
derive instance newtypeTransactionResultResponse :: Newtype TransactionResultResponse _

instance decodeTransactionResultResponse :: Decode TransactionResultResponse where
  decode = genericDecode tendermintOptions

--------------------------------------------------------------------------------

fEither :: F ~> Either String
fEither = runExcept <<<  withExcept show

tendermintOptions :: Options
tendermintOptions = defaultOptions { unwrapSingleConstructors = true }

genericDecoder
  :: forall a.
     Decode a
  => Decoder String a
genericDecoder =
    { parse: Right
    , decode: fEither <<< decodeJSON
    }
