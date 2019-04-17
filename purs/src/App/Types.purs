module App.Types where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (note')
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Network.Ethereum.Web3 (UIntN, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)

newtype Token = Token (UIntN S256)
derive instance newtypeToken :: Newtype Token _
derive instance genericToken :: Generic Token _
instance eqToken :: Eq Token where eq = genericEq
instance ordToken :: Ord Token where compare = genericCompare
instance showToken :: Show Token where show = genericShow

instance encodejsonToken :: EncodeJson Token where
  encodeJson (Token u) = encodeJson $ unUIntN u

instance decodejsonToken :: DecodeJson Token where
  decodeJson = decodeJson >=> \bn -> note'
    (\_ -> "Expected To get valid Token but got: " <> show bn)
      $ Token <$> uIntNFromBigNumber s256 bn
