module Tendermint.Routes where

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadAsk)
import Effect.Aff.Class (class MonadAff)
import Servant.Api.Types (type (:>), GET, QP, QueryParams, Required, RouteProxy(..), S, noCaptures, noHeaders)
import Servant.Client.Client (buildGetRequest)
import Servant.Client.Request (AjaxError, ClientEnv)
import Tendermint.Types as TT


type GetTransactionResult =
  S "tx"
  :> QP ( hash :: Required TT.TransactionHash
        )
  :> GET TT.TransactionResultResponse

getTransactionResult
  :: forall m.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => QueryParams ( hash :: Required TT.TransactionHash
                 )
  -> m TT.TransactionResultResponse
getTransactionResult qps = buildGetRequest (RouteProxy :: RouteProxy GetTransactionResult) noCaptures qps noHeaders TT.genericDecoder

{-
testGetTransactionResponse :: Effect Unit
testGetTransactionResponse = launchAff_ do
  resp <- assertRequest {protocol : "http", baseURL: "//127.0.0.1:26657/"} $ getTransactionResult $
          QueryParams { hash: Required (unsafeCoerce "812C12CE1A43B7D4A239D1D8B7D6207BE1255881097DC1379394A2AC52E410F8")
                      }
  C.logShow resp
-}
