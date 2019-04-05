module Plasma.Routes where

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Plasma.Types as PT
import Servant.Api.Types (type (:>), Capture, Captures, GET, POST, QP, QueryParams, Required, RouteProxy(..), S, noCaptures, noHeaders, noQueryParams)
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

type GetProof =
     S "proof"
  :> QP ( position :: Required PT.Position
        , ownerAddress :: Required PT.EthAddress
        )
  :> GET PT.GetProofResp

getProof
  :: forall m.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => QueryParams ( ownerAddress :: Required PT.EthAddress
                 , position :: Required PT.Position
                 )
  -> m PT.GetProofResp
getProof qps = buildGetRequest (RouteProxy :: RouteProxy GetProof) noCaptures qps noHeaders PT.genericDecoder
