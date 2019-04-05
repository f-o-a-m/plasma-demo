module Plasma.Routes where

import Prelude

import Chanterelle.Test (assertWeb3)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader.Class (class MonadAsk)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as C
import Network.Ethereum.Web3 (httpProvider)
import Plasma.Types as PT
import Plasma.Utils (makeConfirmationSignatureWithNode)
import Servant.Api.Types (type (:>), Capture, Captures, GET, POST, QP, QueryParams(..), Required(..), RouteProxy(..), S, noCaptures, noHeaders, noQueryParams)
import Servant.Client.Client (buildGetRequest, buildPostRequest)
import Servant.Client.Request (AjaxError, ClientEnv, assertRequest)
import Unsafe.Coerce (unsafeCoerce)


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

testGetProof = launchAff_ do
  PT.GetProofResp resp <- assertRequest {protocol : "http", baseURL: "//127.0.0.1:1317/"} $ getProof $
  QueryParams { ownerAddress: Required (unsafeCoerce "11205dbb90321aeb5e6b8a6792f1e83412bb522b")
              , position: Required (PT.Position { blockNumber: 22
                                                , transactionIndex: 0
                                                , outputIndex: 0
                                                , depositNonce: 0
                                                }
                                   )
              }

  utxo <- assertRequest {protocol : "http", baseURL: "//127.0.0.1:1317/"} $ getUTXO $
          QueryParams { ownerAddress: Required (unsafeCoerce "11205dbb90321aeb5e6b8a6792f1e83412bb522b")
                      , position: Required (PT.Position { blockNumber: 22
                                                        , transactionIndex: 0
                                                        , outputIndex: 0
                                                        , depositNonce: 0
                                                        }
                                           )
                      }
  provider <- liftEffect $ httpProvider "http://localhost:8545"
  let creds = { signer: (unsafeCoerce "d77d04fb59675fe5df499fb4096c51edceff5046")
              , password: Just "password123"
              }
  sig <- assertWeb3 provider $ makeConfirmationSignatureWithNode creds utxo
  let args = { txBytes: (un PT.Transaction resp.transaction).tx
             , proof: resp.proofAunts
             , confirmationSignatures: [sig]
             }
  C.logShow args

--------------------------------------------------------------------------------

type GetUTXO =
     S "utxo"
  :> QP ( ownerAddress :: Required PT.EthAddress
        , position :: Required PT.Position
        )
  :> GET PT.UTXO

getUTXO
  :: forall m.
     MonadAsk ClientEnv m
  => MonadError AjaxError m
  => MonadAff m
  => QueryParams ( ownerAddress :: Required PT.EthAddress
                 , position :: Required PT.Position
                 )
  -> m PT.UTXO
getUTXO qps = buildGetRequest (RouteProxy :: RouteProxy GetUTXO) noCaptures qps noHeaders PT.genericDecoder


testGetUTXO = launchAff_ do
  resp <- assertRequest {protocol : "http", baseURL: "//127.0.0.1:1317/"} $ getUTXO $
            QueryParams { ownerAddress: Required (unsafeCoerce "11205dbb90321aeb5e6b8a6792f1e83412bb522b")
                        , position: Required (PT.Position { blockNumber: 22
                                                          , transactionIndex: 0
                                                          , outputIndex: 0
                                                          , depositNonce: 0
                                                          }
                                              )
                        }
  C.logShow resp
