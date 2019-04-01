module Plasma.Types where

import Prelude
import Control.Monad.Except (runExcept, withExcept)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Foreign (F)
import Foreign.Generic (genericDecodeJSON, defaultOptions)
import Foreign.Generic.Class (class GenericDecode)
import Foreign.Generic.Types (Options)

newtype Position =
  Position { blockNumber :: Int
           , transactionIndex :: Int
           , outputIndex :: Int
           , depositNonce :: Int
           }

derive instance genericPosition :: Generic Position _

newtype UTXO =
  UTXO { inputKeys :: Array String
       , spenderKeys :: Array String
       , confirmationHash :: String
       , "MerkleHash" :: String
       , spent :: Boolean
       , position :: Position
       }

derive instance genericUTXO :: Generic UTXO _

{-
  {
    "inputKeys": [
       "Bxn3mt+qKwWnXHPG8ScYetLeaYXEgICABQ=="
       ],
    "spenderKeys": [],
    "confirmationHash": "/sSdrfPv4wIzAYwjgYBgm2wO2uLO9dGpqZ6AbzS9oLI=",
    "MerkleHash": "KO9FML0lhV06regzZQXFZUGgaoGOegzS3/bER9XnShA=",
    "ouput": {
      "owner": "snz0+jO/1DEtlAOL6JsNPKyGvcA=",
      "amount": 1000
      },
    "spent": false,
    "position": {
      "blockNumber": 9,
      "transactionIndex": 0,
      "outputIndex": 0,
      "depositNonce": 0
      }
  }
-}

fEither :: F ~> Either String
fEither = runExcept <<<  withExcept show

plasmaOptions :: Options
plasmaOptions = defaultOptions { unwrapSingleConstructors = true }

genericDecoder
  :: forall a rep.
     Generic a rep
  => GenericDecode rep
  => String
  -> Either String a
genericDecoder = fEither <<< genericDecodeJSON plasmaOptions
