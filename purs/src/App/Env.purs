module App.Env
  ( Env
  , make
  )
  where

import Prelude

import App.Ethereum.Provider as EthereumProvider
import App.Utils.Network (Network, NodeURL, getNodeNetwork, getNodeURL)
import Effect.Aff.BRef (BRefR)
import Effect.Class (class MonadEffect, liftEffect)

type Env =
  { nodeURL :: NodeURL
  , nodeNetwork :: Network
  , providerState :: BRefR EthereumProvider.State
  }

make
  :: forall m
   . MonadEffect m
  => m Env
make = liftEffect do
  nodeURL <- getNodeURL
  nodeNetwork <- getNodeNetwork
  providerState <- EthereumProvider.getEnabledEthereumProviderState
  pure
    { nodeURL
    , nodeNetwork
    , providerState
    }
