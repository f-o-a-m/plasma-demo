module App.Component.Provider where

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
  , providerState :: EthereumProvider.State
  }

type Input = Unit
type Message = Void
type DSL m = H.ComponentDSL State Query Message m
type HTML = H.ComponentHTML Query


component
  :: forall m
   . EthereumProvider.State
   -> H.Component HH.HTML Query Input Message m
component providerState =
  H.lifecycleComponent
    { initialState: \_ ->
        { balance: Nothing
        , providerState
        }
    , render
    , eval
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where
    render :: State -> HTML
    render _ =
      HH.div_
      [ HH.h1_
        [ HH.text "Provider" ]
      ]

    eval
      :: Query ~> DSL m
    eval = case _ of
      Initialize next -> do
        pure next
