module Spec.Plasma.Utils where

import Prelude

import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff (error, forkAff, killFiber)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Network.Ethereum.Web3 (class EventFilter, Address, BigNumber, EventAction(..), HexString, Provider, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, event, eventFilter, forkWeb3', uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Event (class DecodeEvent)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy)


takeEventOrFail
  :: forall ev i ni.
     DecodeEvent i ni ev
  => EventFilter ev
  => Proxy ev
  -> Provider
  -> Address
  -> Web3 HexString
  -> Web3 (Either HexString ev)
takeEventOrFail prx provider addrs web3Action = do
    var <- liftAff AVar.empty
    let eventWatcher = do
          event (eventFilter prx addrs) $ \e -> do
            liftAff $ AVar.put (Right e) var
            pure TerminateEvent
    f1 <- forkWeb3' eventWatcher
    txHash <- web3Action
    let failureWatcher = checkForFailure txHash var
    f2 <- liftAff $ forkAff $ checkForFailure txHash var
    res <- liftAff $ AVar.take var
    -- clean up both fibers, cause we don't know which is already done.
    liftAff $ traverse_ (killFiber (error "done")) [f1, Right <$> f2]
    pure res
  where
    checkForFailure txHash var = do
      TransactionReceipt {status} <- pollTransactionReceipt txHash provider
      case status of
        Failed -> do
          AVar.put (Left txHash) var
        _ -> pure unit


--------------------------------------------------------------------------------

unsafeMkUInt256 :: BigNumber -> UIntN S256
unsafeMkUInt256 n = unsafeFromJust (show n <> " is a valid uint256") $ uIntNFromBigNumber s256 n

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust msg = case _ of
  Nothing -> unsafeCrashWith $ "unsafeFromJust: " <> msg
  Just a -> a
