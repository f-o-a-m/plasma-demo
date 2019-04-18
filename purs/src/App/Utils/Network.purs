module App.Utils.Network where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Effect (Effect)
import Foreign (Foreign, readString)
import Partial.Unsafe (unsafeCrashWith)

foreign import nodeURLImpl :: Effect Foreign

newtype NodeURL = NodeURL String

getNodeURL :: Effect NodeURL
getNodeURL = nodeURLImpl <#> \url -> case runExcept $ readString url of
    Left err -> unsafeCrashWith $ "Missing ENV to get node url." <> show err
    Right url' -> NodeURL url'

foreign import nodeNetworkIdImpl :: Effect Foreign

getNodeNetwork :: Effect Network
getNodeNetwork = nodeNetworkIdImpl <#> \nId -> case runExcept $ readString nId of
    Left err -> unsafeCrashWith $ "Missing ENV to get node's network id" <> show err
    Right nId' -> networkFromId nId'

data Network
  = Mainnet
  | Rinkeby
  | Cliquebait
  | Unknown

derive instance eqNetwork :: Eq Network
derive instance genericNetwork :: Generic Network _
instance showNetwork :: Show Network where show = genericShow

networkFromId :: String -> Network
networkFromId = case _ of
  "1"      -> Mainnet
  "4"      -> Rinkeby
  "420123" -> Cliquebait
  _        -> Unknown

networkName :: Network -> String
networkName Mainnet    = "Main Ethereum Network"
networkName Rinkeby    = "Rinkeby Testnet"
networkName Cliquebait = "Cliquebait Dreamnet"
networkName Unknown    = "Unsupported Network"
