module App.Ethereum.Provider
  ( State(..)
  , NotConnectedState
  , ConnectedState
  , viewConnectedState
  , getEnabledEthereumProviderState
  ) where


import Prelude

import App.Utils.Network (Network, networkFromId)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_, parallel, sequential, try)
import Effect.Aff.AVar.RW as AVar
import Effect.Aff.BRef (BRefR, BRefRW)
import Effect.Aff.BRef as BRef
import Effect.Class (liftEffect)
import Effect.Exception (Error, catchException)
import Effect.Uncurried (EffectFn1, EffectFn4, mkEffectFn1, runEffectFn4)
import Network.Ethereum.Web3 (Address, Provider, Web3, Web3Error, metamaskProvider, runWeb3)
import Network.Ethereum.Web3.Api (eth_getAccounts, net_version)


data State
  = NotInjected
  | PendingApproval
  | Rejected Error
  | Loading
  | Connected ConnectedState
  | NotConnected NotConnectedState
  | Web3DataError Web3Error

type ConnectedState = { userAddress :: Address, provider :: Provider }
type NotConnectedState = { currentNetwork :: Network, userAddress :: Maybe Address }

partialStateEq :: State -> State -> Boolean
partialStateEq a b = case a, b of
  NotInjected, NotInjected -> true
  NotInjected, _ -> false
  Rejected _, Rejected _ -> true
  Rejected _, _ -> false
  PendingApproval, PendingApproval -> true
  PendingApproval, _ -> false
  Loading, Loading -> true
  Loading, _ -> false
  Connected s, Connected s' -> s.userAddress == s'.userAddress
  Connected s, _ -> false
  NotConnected s, NotConnected s' -> s.currentNetwork == s'.currentNetwork && s.userAddress == s'.userAddress
  NotConnected s, _ -> false
  Web3DataError _, Web3DataError _ -> true
  Web3DataError _, _ -> false

viewConnectedState :: State -> Maybe ConnectedState
viewConnectedState (Connected s) = Just s
viewConnectedState _ = Nothing

viewNotConnectedState :: State -> Maybe NotConnectedState
viewNotConnectedState (NotConnected s) = Just s
viewNotConnectedState _ = Nothing

-- https://medium.com/metamask/https-medium-com-metamask-breaking-change-injecting-web3-7722797916a8
getEnabledEthereumProviderState :: Effect (BRefR State)
getEnabledEthereumProviderState = do
  Tuple accountVarR accountVarW <- AVar.split <$> AVar.empty
  runEffectFn4 getEnabledEthereumProvider_
    Just
    Nothing
    (mkEffectFn1 $ launchAff_ <<< flip AVar.put accountVarW)
    (mkEffectFn1 $ launchAff_ <<< flip AVar.kill accountVarW)
    >>= case _ of
      Nothing -> catchException (const $ pure Nothing) (Just <$> metamaskProvider) >>= case _ of
        Just provider -> do
          stateBRef <- BRef.make Loading
          launchAff_ $ connectionLoop provider stateBRef
          pure $ fst $ BRef.split $ stateBRef
        Nothing -> do
          stateBRef <- BRef.make NotInjected
          pure $ fst $ BRef.split $ stateBRef
      Just provider -> do
        stateBRef <- BRef.make PendingApproval
        launchAff_ $ try (AVar.read accountVarR) >>= case _ of
          Left err ->
            Rejected err `BRef.write` stateBRef
          Right _ -> do
            Loading `BRef.write` stateBRef
            connectionLoop provider stateBRef
        pure $ fst $ BRef.split $ stateBRef
  where
      loadNetworkAndUserAddress :: Web3 (Tuple Network (Maybe Address))
      loadNetworkAndUserAddress = sequential $ Tuple
        <$> parallel (net_version <#> networkFromId)
        <*> parallel (eth_getAccounts <#> (_ !! 0))

      connectionLoop :: Provider -> BRefRW State -> Aff Unit
      connectionLoop provider stateBRef = do
        currentState <- liftEffect $ BRef.read stateBRef
        runWeb3 provider loadNetworkAndUserAddress >>= case _ of
          Left err -> do
            Web3DataError err `BRef.write` stateBRef
          Right (Tuple currentNetwork mbUserAddress) ->
            let
              newSt = case mbUserAddress of
                Nothing -> NotConnected { currentNetwork, userAddress: Nothing }
                Just userAddress -> Connected { userAddress, provider }
            in unless (currentState `partialStateEq` newSt) $ newSt `BRef.write` stateBRef
        delay $ Milliseconds 1000.0
        connectionLoop provider stateBRef

foreign import getEnabledEthereumProvider_ :: forall res. EffectFn4
  (Provider -> res)
  res
  (EffectFn1 (Array Address) Unit)
  (EffectFn1 Error Unit)
  res
