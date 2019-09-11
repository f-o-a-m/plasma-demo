module Spec.Plasma.Utils where

import Prelude

import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Control.Monad.Reader (ask)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (over2)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Fiber, joinFiber)
import Effect.Aff (Milliseconds(..), delay, error, forkAff, killFiber)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Class (liftEffect)
import Effect.Console as C
import Network.Ethereum.Web3 (Change(..), runWeb3, forkWeb3, Web3Error, Filter, class EventFilter, Address, BigNumber, BlockNumber(..), Change, EventAction(..), HexString, Provider, TransactionReceipt(..), TransactionStatus(..), UIntN, Web3, event, eventFilter, forkWeb3', uIntNFromBigNumber)
import Network.Ethereum.Web3.Api (eth_blockNumber)
import Network.Ethereum.Web3.Solidity.Event (class DecodeEvent)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Partial.Unsafe (unsafeCrashWith)
import Type.Proxy (Proxy)

type Logger m = String -> m Unit

joinWeb3Fork
  :: forall a m.
     MonadAff m
  => Fiber (Either Web3Error a)
  -> m a
joinWeb3Fork fiber = liftAff do
  eRes <- joinFiber fiber
  case eRes of
    Left e -> unsafeCrashWith $ "Error in forked web3 process " <> show e
    Right a -> pure a


assertWeb3
  :: forall m a.
     MonadAff m
  => Provider
  -> Web3 a
  -> m a
assertWeb3 provider a = liftAff $ runWeb3 provider a <#> case _ of
  Right x -> x
  Left err -> unsafeCrashWith $ "expected Right in `assertWeb3`, got error" <> show err

-- special event listener
monitorUntil
  :: forall m e i ni.
     MonadAff m
  => DecodeEvent i ni e
  => Provider
  -> Logger m
  -> Web3 HexString
  -> Filter e
  -> m e
monitorUntil provider logger action filter = liftAff do
  valueV <- AVar.empty
  txHashV <- AVar.empty
  -- find the right trx via its hash
  let handler eventValue = do
        (Change e) <- ask
        txHash <- liftAff $ AVar.read txHashV
        if e.transactionHash == txHash
          then do
            liftAff $ AVar.put eventValue valueV
            pure TerminateEvent
          else pure ContinueEvent
  -- start filter
  fiber <- forkWeb3 provider do
    event filter handler
  -- launch action and record its txHash
  txHash <- assertWeb3 provider action
  AVar.put txHash txHashV
  -- join the fiber
  _ <- joinWeb3Fork fiber
  AVar.take valueV

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

-- | Wait for a specific block number
waitForBlock
  :: BlockNumber
  -> Web3 Unit
waitForBlock bn = do
  currentBlock <- eth_blockNumber
  if currentBlock >= bn
    then do
      liftEffect $ C.log $ "🔗 Receiving block " <> show currentBlock
      pure unit
    else do
      liftEffect $ C.log ("Waiting for block " <> show bn <> " (current: " <> show currentBlock <>") ...")
      liftAff (delay $ Milliseconds 1000.0)
      waitForBlock bn

-- | Wait for number of blocks
waitForBlocks
  :: BlockNumber
  -> Web3 Unit
waitForBlocks offset =
  over2 BlockNumber (+) offset <$> eth_blockNumber
    >>= waitForBlock

--------------------------------------------------------------------------------

unsafeMkUInt256 :: BigNumber -> UIntN S256
unsafeMkUInt256 n = unsafeFromJust (show n <> " is a valid uint256") $ uIntNFromBigNumber s256 n

unsafeFromJust :: forall a. String -> Maybe a -> a
unsafeFromJust msg = case _ of
  Nothing -> unsafeCrashWith $ "unsafeFromJust: " <> msg
  Just a -> a

defaultPassword :: String
defaultPassword = "password123"
