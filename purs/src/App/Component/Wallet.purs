module App.Wallet.Component where

import Prelude

import App.Component.Account as Account
import App.Component.Deposit as Deposit
import App.Component.Provider as Provider
import App.Component.Spend as Spend
import App.Env as App
import App.Ethereum.Provider (viewConnectedState)
import App.Ethereum.Provider as EthereumProvider
import App.Utils.Halogen (maybeHtml)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Either.Nested (Either4)
import Data.Functor.Coproduct.Nested (Coproduct4)
import Data.Maybe (Maybe(..))
import Effect.Aff.BRef as BRef
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource.Extras as HES
import Network.Ethereum.Web3.Types.TokenUnit (kind Token)


data Query a
  = Initialize a
  | ProviderStateChange EthereumProvider.State a
  | ChangeViewState ViewState a

data ViewState
  = DepositView
  | SpendView

type State =
  { viewState :: ViewState
  , providerState :: Maybe EthereumProvider.State
  }

type Input = Unit
type Message = Void

type DSL m = H.ParentDSL State Query ChildQuery ChildSlot Message m

type ChildQuery = Coproduct4 Provider.Query Account.Query Deposit.Query Spend.Query
type ChildSlot = Either4 Unit Unit Unit Unit

providerPath :: CP.ChildPath Provider.Query ChildQuery Unit ChildSlot
providerPath = CP.cp1

accountPath :: CP.ChildPath Account.Query ChildQuery Unit ChildSlot
accountPath = CP.cp2

depositPath :: CP.ChildPath Deposit.Query ChildQuery Unit ChildSlot
depositPath = CP.cp3

spendPath :: CP.ChildPath Spend.Query ChildQuery Unit ChildSlot
spendPath = CP.cp4

initialState :: State
initialState =
  { viewState: DepositView
  , providerState: Nothing
  }

component
  :: forall m
   . MonadAsk App.Env m
  => MonadAff m
  => H.Component HH.HTML Query Input Message m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    render
      :: MonadAff m
      => MonadAsk App.Env m
      => State -> H.ParentHTML Query ChildQuery ChildSlot m
    render { viewState, providerState } =
      HH.div_
      [ maybeHtml providerState \pSt ->
          HH.slot' providerPath unit (Provider.component pSt) unit absurd
      , maybeHtml (providerState >>= viewConnectedState) \connectedState ->
          HH.slot'
            accountPath
            unit
            (Account.component connectedState)
            unit
            absurd
      , HH.button
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

    eval
      :: MonadAsk App.Env m
      => MonadAff m
      => Query ~> DSL m
    eval = case _ of
      Initialize next -> do
        { providerState } <- ask
        providerState' <- BRef.read providerState
        H.modify_ _{ providerState = Just providerState' }
        H.subscribe $ HES.eventSourceAff
          $ BRef.readOnUpdate providerState
          <#> ProviderStateChange >>> pure >>> HES.listening
        pure next
      ProviderStateChange newProviderState next -> do
        H.modify_ _{ providerState = Just newProviderState }
        pure next
      ChangeViewState viewState next -> do
        H.modify_ _{ viewState = viewState }
        pure next
