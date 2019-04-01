module Plasma.Routes where

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Plasma.Types as PT
import Servant.Api.Types (type (:>), Capture, Captures, GET, POST, RouteProxy(..), S, noCaptures, noHeaders, noQueryParams)
import Servant.Client.Client (buildGetRequest, buildPostRequest)
import Servant.Client.Request (AjaxError, ClientEnv)


type GetHealth =
     S "health"
  :> GET String

getHealth
  :: forall m.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => m String
getHealth = buildGetRequest (RouteProxy :: RouteProxy GetHealth) noCaptures noQueryParams noHeaders PT.genericDecoder


--------------------------------------------------------------------------------

type GetUTXOs =
     S "balance"
  :> Capture "owner" PT.EthAddress
  :> GET (Array PT.UTXO)

getUTXOs
  :: forall m.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => Captures (owner :: PT.EthAddress)
  -> m (Array PT.UTXO)
getUTXOs captures =
  buildGetRequest (RouteProxy :: RouteProxy GetUTXOs) captures noQueryParams noHeaders PT.genericDecoder

--------------------------------------------------------------------------------

type PostIncludeDeposit =
     S "deposit"
  :> S "include"
  :> POST PT.PostDepositBody String

postIncludeDeposit
  :: forall m.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => PT.PostDepositBody
  -> m String
postIncludeDeposit body =
  buildPostRequest (RouteProxy :: RouteProxy PostIncludeDeposit) noCaptures body noQueryParams
    noHeaders PT.genericDecoder PT.genericEncoder

--------------------------------------------------------------------------------
