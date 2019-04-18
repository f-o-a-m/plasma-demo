module App.Component.Account where

import Prelude

import App.Ethereum.Provider as EthereumProvider
import App.Types (Token)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH


data Query a
  = Initialize a

type State =
  { balance :: Maybe Token
  , connectedState :: EthereumProvider.ConnectedState
  }

type Input = Unit
type Message = Void
type DSL m = H.ComponentDSL State Query Message m
type HTML = H.ComponentHTML Query


component
  :: forall m
   . EthereumProvider.ConnectedState
  -> H.Component HH.HTML Query Input Message m
component connectedState =
  H.lifecycleComponent
    { initialState: \_ ->
          { balance: Nothing
          , connectedState
          }
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where
    render :: State -> HTML
    render st =
      HH.div_
      [ HH.h1_
        [ HH.text "Account" ]
      , HH.p_
          [ HH.text $ "User Address: " <> show st.connectedState.userAddress
          ]
      ]

    eval
      :: Query ~> DSL m
    eval = case _ of
      Initialize next ->
        pure next
