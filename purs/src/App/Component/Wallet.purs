module App.Wallet.Component where

import Prelude

import App.Component.Deposit as Deposit
import App.Component.Spend as Spend
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Network.Ethereum.Web3.Types.TokenUnit (kind Token)


data Query a
  = Initialize a
  | ChangeViewState ViewState a

data ViewState
  = DepositView
  | SpendView

type State = { viewState :: ViewState }

type Input = Unit
type Message = Void

type DSL m = H.ParentDSL State Query ChildQuery ChildSlot Message m

type ChildQuery = Coproduct2 Deposit.Query Spend.Query
type ChildSlot = Either2 Unit Unit

depositPath :: CP.ChildPath Deposit.Query ChildQuery Unit ChildSlot
depositPath = CP.cp1

spendPath :: CP.ChildPath Spend.Query ChildQuery Unit ChildSlot
spendPath = CP.cp2

initialState :: State
initialState = { viewState: DepositView }

component :: forall m. H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
  }
  where
    render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
    render { viewState } =
      HH.div_
      [ HH.button
        [ HE.onClick $ HE.input_ $ ChangeViewState DepositView
        ]
        [ HH.text "Deposit"]
      , HH.text " | "
      , HH.button
        [ HE.onClick $ HE.input_ $ ChangeViewState SpendView
        ]
        [ HH.text "Spend"]
      , HH.div_
        [case viewState of
            DepositView -> HH.slot' depositPath unit Deposit.component unit absurd
            SpendView -> HH.slot' spendPath unit Spend.component unit absurd
        ]
      ]

    eval :: Query ~> DSL m
    eval = case _ of
      Initialize next ->
        pure next
      ChangeViewState viewState next -> do
        H.modify_ _{ viewState = viewState }
        pure next
