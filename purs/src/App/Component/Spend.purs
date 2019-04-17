module App.Component.Spend where

import Prelude

import App.Types (Token)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE


data Query a
  = Initialize a
  | SubmitSpend a

type State = { deposit :: Maybe Token }

initialState :: State
initialState = { deposit: Nothing }

type Input = Unit
type Message = Void
type DSL m = H.ComponentDSL State Query Message m
type HTML = H.ComponentHTML Query


component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.lifecycleComponent
    { initialState: const initialState
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
        [ HH.text "Spend"]
      , HH.button
        [ HE.onClick $ HE.input_ $ SubmitSpend
        ]
        [ HH.text "Submit"]
      ]

    eval :: Query ~> DSL m
    eval = case _ of
      Initialize next ->
        pure next
      SubmitSpend next ->
        pure next
