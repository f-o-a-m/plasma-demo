module Spec.Plasma.Utils where

import Prelude

import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Milliseconds(..), delay, error, forkAff, killFiber)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console as C
import Network.Ethereum.Web3 (class EventFilter, Address, BigNumber, BlockNumber, Change, EventAction(..), HexString, Provider, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, event, eventFilter, forkWeb3', uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_blockNumber)
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
  -> Web3 (Either HexString (Tuple Change ev))
takeEventOrFail prx provider addrs web3Action = do
    var <- liftAff AVar.empty
    let eventWatcher = do
          event (eventFilter prx addrs) $ \e -> do
            change <- ask
            liftAff $ AVar.put (Right (Tuple change e)) var
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

waitForBlock
  :: BlockNumber
  -> Web3 Unit
waitForBlock bn = do
  currentBlock <- eth_blockNumber
  if currentBlock >= bn
     then pure unit
     else do
       liftEffect $ C.log ("Waiting for BlockNumber " <> show bn <> "...")
       liftAff (delay $ Milliseconds 1000.0)
       waitForBlock bn

--------------------------------------------------------------------------------

unsafeMkUInt256 :: BigNumber -> UIntN S256
unsafeMkUInt256 n = unsafeFromJust (show n <> " is a valid uint256") $ uIntNFromBigNumber s256 n

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust msg = case _ of
  Nothing -> unsafeCrashWith $ "unsafeFromJust: " <> msg
  Just a -> a

defaultPassword :: String
defaultPassword = "password123"
