--------------------------------------------------------------------------------
-- | PlasmaMVP
--------------------------------------------------------------------------------

module Contracts.PlasmaMVP where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((.~))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (Vector, _address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D2, D3, D4, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3, Tuple4(..), Tuple5(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | ChangedOperator
--------------------------------------------------------------------------------


newtype ChangedOperator = ChangedOperator {oldOperator :: Address,newOperator :: Address}

derive instance newtypeChangedOperator :: Newtype ChangedOperator _

instance eventFilterChangedOperator :: EventFilter ChangedOperator where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "3aff32e6289f2f2a2463481071051456b2768bb391c64ae3c91f9033e208cda1")]

instance indexedEventChangedOperator :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "oldOperator") Address) (Tagged (SProxy "newOperator") Address)) ChangedOperator where
  isAnonymous _ = false

derive instance genericChangedOperator :: Generic ChangedOperator _

instance eventGenericChangedOperatorShow :: Show ChangedOperator where
	show = genericShow

instance eventGenericChangedOperatoreq :: Eq ChangedOperator where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AddedToBalances
--------------------------------------------------------------------------------


newtype AddedToBalances = AddedToBalances {owner :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeAddedToBalances :: Newtype AddedToBalances _

instance eventFilterAddedToBalances :: EventFilter AddedToBalances where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "f8552a24c7d58fd05114f6fc9db7b3a354db64d5fc758184af1696ccd8f158f3")]

instance indexedEventAddedToBalances :: IndexedEvent (Tuple0 ) (Tuple2 (Tagged (SProxy "owner") Address) (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) AddedToBalances where
  isAnonymous _ = false

derive instance genericAddedToBalances :: Generic AddedToBalances _

instance eventGenericAddedToBalancesShow :: Show AddedToBalances where
	show = genericShow

instance eventGenericAddedToBalanceseq :: Eq AddedToBalances where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BlockSubmitted
--------------------------------------------------------------------------------


newtype BlockSubmitted = BlockSubmitted {header :: (BytesN (D3 :& DOne D2)),blockNumber :: (UIntN (D2 :& D5 :& DOne D6)),numTxns :: (UIntN (D2 :& D5 :& DOne D6)),feeAmount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeBlockSubmitted :: Newtype BlockSubmitted _

instance eventFilterBlockSubmitted :: EventFilter BlockSubmitted where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "044ff3798f9b3ad55d1155cea9a40508c71b4c64335f5dae87e8e11551515a06")]

instance indexedEventBlockSubmitted :: IndexedEvent (Tuple0 ) (Tuple4 (Tagged (SProxy "header") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "numTxns") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "feeAmount") (UIntN (D2 :& D5 :& DOne D6)))) BlockSubmitted where
  isAnonymous _ = false

derive instance genericBlockSubmitted :: Generic BlockSubmitted _

instance eventGenericBlockSubmittedShow :: Show BlockSubmitted where
	show = genericShow

instance eventGenericBlockSubmittedeq :: Eq BlockSubmitted where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Deposit
--------------------------------------------------------------------------------


newtype Deposit = Deposit {depositor :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6)),depositNonce :: (UIntN (D2 :& D5 :& DOne D6)),ethBlockNum :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeDeposit :: Newtype Deposit _

instance eventFilterDeposit :: EventFilter Deposit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "36af321ec8d3c75236829c5317affd40ddb308863a1236d2d277a4025cccee1e")]

instance indexedEventDeposit :: IndexedEvent (Tuple0 ) (Tuple4 (Tagged (SProxy "depositor") Address) (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "depositNonce") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "ethBlockNum") (UIntN (D2 :& D5 :& DOne D6)))) Deposit where
  isAnonymous _ = false

derive instance genericDeposit :: Generic Deposit _

instance eventGenericDepositShow :: Show Deposit where
	show = genericShow

instance eventGenericDepositeq :: Eq Deposit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | StartedTransactionExit
--------------------------------------------------------------------------------


newtype StartedTransactionExit = StartedTransactionExit {position :: (Vector (DOne D3) (UIntN (D2 :& D5 :& DOne D6))),owner :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6)),confirmSignatures :: ByteString,committedFee :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeStartedTransactionExit :: Newtype StartedTransactionExit _

instance eventFilterStartedTransactionExit :: EventFilter StartedTransactionExit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "20d695720ae96d3511520c6f51d6ab23aa19a3796da77024ad027b344bb72530")]

instance indexedEventStartedTransactionExit :: IndexedEvent (Tuple0 ) (Tuple5 (Tagged (SProxy "position") (Vector (DOne D3) (UIntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "owner") Address) (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "confirmSignatures") ByteString) (Tagged (SProxy "committedFee") (UIntN (D2 :& D5 :& DOne D6)))) StartedTransactionExit where
  isAnonymous _ = false

derive instance genericStartedTransactionExit :: Generic StartedTransactionExit _

instance eventGenericStartedTransactionExitShow :: Show StartedTransactionExit where
	show = genericShow

instance eventGenericStartedTransactionExiteq :: Eq StartedTransactionExit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | StartedDepositExit
--------------------------------------------------------------------------------


newtype StartedDepositExit = StartedDepositExit {nonce :: (UIntN (D2 :& D5 :& DOne D6)),owner :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6)),committedFee :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeStartedDepositExit :: Newtype StartedDepositExit _

instance eventFilterStartedDepositExit :: EventFilter StartedDepositExit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e90dc7204eb622c87d2c8d67d8e27afdfd34042584591e7b3d35014873cf9cfd")]

instance indexedEventStartedDepositExit :: IndexedEvent (Tuple0 ) (Tuple4 (Tagged (SProxy "nonce") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "owner") Address) (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "committedFee") (UIntN (D2 :& D5 :& DOne D6)))) StartedDepositExit where
  isAnonymous _ = false

derive instance genericStartedDepositExit :: Generic StartedDepositExit _

instance eventGenericStartedDepositExitShow :: Show StartedDepositExit where
	show = genericShow

instance eventGenericStartedDepositExiteq :: Eq StartedDepositExit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | ChallengedExit
--------------------------------------------------------------------------------


newtype ChallengedExit = ChallengedExit {position :: (Vector (DOne D4) (UIntN (D2 :& D5 :& DOne D6))),owner :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeChallengedExit :: Newtype ChallengedExit _

instance eventFilterChallengedExit :: EventFilter ChallengedExit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "e1289dafb1083e540206bcd7d95a9705ba2590d6a9229c35a1c4c4c5efbda901")]

instance indexedEventChallengedExit :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "position") (Vector (DOne D4) (UIntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "owner") Address) (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) ChallengedExit where
  isAnonymous _ = false

derive instance genericChallengedExit :: Generic ChallengedExit _

instance eventGenericChallengedExitShow :: Show ChallengedExit where
	show = genericShow

instance eventGenericChallengedExiteq :: Eq ChallengedExit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | FinalizedExit
--------------------------------------------------------------------------------


newtype FinalizedExit = FinalizedExit {position :: (Vector (DOne D4) (UIntN (D2 :& D5 :& DOne D6))),owner :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeFinalizedExit :: Newtype FinalizedExit _

instance eventFilterFinalizedExit :: EventFilter FinalizedExit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "b5083a27a38f8a9aa999efb3306b7be96dc3f42010a968dd86627880ba7fdbe2")]

instance indexedEventFinalizedExit :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "position") (Vector (DOne D4) (UIntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "owner") Address) (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) FinalizedExit where
  isAnonymous _ = false

derive instance genericFinalizedExit :: Generic FinalizedExit _

instance eventGenericFinalizedExitShow :: Show FinalizedExit where
	show = genericShow

instance eventGenericFinalizedExiteq :: Eq FinalizedExit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BalanceOfFn
--------------------------------------------------------------------------------


type BalanceOfFn = Tagged (SProxy "balanceOf(address)") (Tuple1 (Tagged (SProxy "_address") Address))

balanceOf :: TransactionOptions NoPay -> ChainCursor -> { _address :: Address } -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
balanceOf x0 cm r = uncurryFields  r $ balanceOf' x0 cm
   where
    balanceOf' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_address") Address) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
    balanceOf' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: BalanceOfFn)

--------------------------------------------------------------------------------
-- | BalancesFn
--------------------------------------------------------------------------------


type BalancesFn = Tagged (SProxy "balances(address)") (Tuple1 Address)

balances :: TransactionOptions NoPay -> ChainCursor -> Address -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
balances x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: BalancesFn)

--------------------------------------------------------------------------------
-- | ChallengeExitFn
--------------------------------------------------------------------------------


type ChallengeExitFn = Tagged (SProxy "challengeExit(uint256[4],uint256[2],bytes,bytes,bytes)") (Tuple5 (Tagged (SProxy "exitingTxPos") (Vector (DOne D4) (UIntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "challengingTxPos") (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "txBytes") ByteString) (Tagged (SProxy "proof") ByteString) (Tagged (SProxy "confirmSignature") ByteString))

challengeExit :: TransactionOptions NoPay -> { exitingTxPos :: (Vector (DOne D4) (UIntN (D2 :& D5 :& DOne D6))), challengingTxPos :: (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))), txBytes :: ByteString, proof :: ByteString, confirmSignature :: ByteString } -> Web3 HexString
challengeExit x0 r = uncurryFields  r $ challengeExit' x0
   where
    challengeExit' :: TransactionOptions NoPay -> (Tagged (SProxy "exitingTxPos") (Vector (DOne D4) (UIntN (D2 :& D5 :& DOne D6)))) -> (Tagged (SProxy "challengingTxPos") (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6)))) -> (Tagged (SProxy "txBytes") ByteString) -> (Tagged (SProxy "proof") ByteString) -> (Tagged (SProxy "confirmSignature") ByteString) -> Web3 HexString
    challengeExit' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 y1 y2 y3 y4 y5) :: ChallengeExitFn)

--------------------------------------------------------------------------------
-- | ChangeOperatorFn
--------------------------------------------------------------------------------


type ChangeOperatorFn = Tagged (SProxy "changeOperator(address)") (Tuple1 (Tagged (SProxy "newOperator") Address))

changeOperator :: TransactionOptions NoPay -> { newOperator :: Address } -> Web3 HexString
changeOperator x0 r = uncurryFields  r $ changeOperator' x0
   where
    changeOperator' :: TransactionOptions NoPay -> (Tagged (SProxy "newOperator") Address) -> Web3 HexString
    changeOperator' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: ChangeOperatorFn)

--------------------------------------------------------------------------------
-- | DepositFn
--------------------------------------------------------------------------------


type DepositFn = Tagged (SProxy "deposit(address)") (Tuple1 (Tagged (SProxy "owner") Address))

deposit :: TransactionOptions MinorUnit -> { owner :: Address } -> Web3 HexString
deposit x0 r = uncurryFields  r $ deposit' x0
   where
    deposit' :: TransactionOptions MinorUnit -> (Tagged (SProxy "owner") Address) -> Web3 HexString
    deposit' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: DepositFn)

--------------------------------------------------------------------------------
-- | DepositExitQueueFn
--------------------------------------------------------------------------------


type DepositExitQueueFn = Tagged (SProxy "depositExitQueue(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

depositExitQueue :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
depositExitQueue x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: DepositExitQueueFn)

--------------------------------------------------------------------------------
-- | DepositExitsFn
--------------------------------------------------------------------------------


type DepositExitsFn = Tagged (SProxy "depositExits(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

depositExits :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (Tuple5 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (DOne D8))))
depositExits x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: DepositExitsFn)

--------------------------------------------------------------------------------
-- | DepositNonceFn
--------------------------------------------------------------------------------


type DepositNonceFn = Tagged (SProxy "depositNonce()") (Tuple0 )

depositNonce :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
depositNonce x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DepositNonceFn)

--------------------------------------------------------------------------------
-- | DepositQueueLengthFn
--------------------------------------------------------------------------------


type DepositQueueLengthFn = Tagged (SProxy "depositQueueLength()") (Tuple0 )

depositQueueLength :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
depositQueueLength x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: DepositQueueLengthFn)

--------------------------------------------------------------------------------
-- | DepositsFn
--------------------------------------------------------------------------------


type DepositsFn = Tagged (SProxy "deposits(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

deposits :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (Tuple4 Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
deposits x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: DepositsFn)

--------------------------------------------------------------------------------
-- | FinalizeDepositExitsFn
--------------------------------------------------------------------------------


type FinalizeDepositExitsFn = Tagged (SProxy "finalizeDepositExits()") (Tuple0 )

finalizeDepositExits :: TransactionOptions NoPay -> Web3 HexString
finalizeDepositExits x0 = sendTx x0 ((tagged $ Tuple0 ) :: FinalizeDepositExitsFn)

--------------------------------------------------------------------------------
-- | FinalizeTransactionExitsFn
--------------------------------------------------------------------------------


type FinalizeTransactionExitsFn = Tagged (SProxy "finalizeTransactionExits()") (Tuple0 )

finalizeTransactionExits :: TransactionOptions NoPay -> Web3 HexString
finalizeTransactionExits x0 = sendTx x0 ((tagged $ Tuple0 ) :: FinalizeTransactionExitsFn)

--------------------------------------------------------------------------------
-- | LastCommittedBlockFn
--------------------------------------------------------------------------------


type LastCommittedBlockFn = Tagged (SProxy "lastCommittedBlock()") (Tuple0 )

lastCommittedBlock :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
lastCommittedBlock x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: LastCommittedBlockFn)

--------------------------------------------------------------------------------
-- | MinExitBondFn
--------------------------------------------------------------------------------


type MinExitBondFn = Tagged (SProxy "minExitBond()") (Tuple0 )

minExitBond :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
minExitBond x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: MinExitBondFn)

--------------------------------------------------------------------------------
-- | OperatorFn
--------------------------------------------------------------------------------


type OperatorFn = Tagged (SProxy "operator()") (Tuple0 )

operator :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
operator x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OperatorFn)

--------------------------------------------------------------------------------
-- | PlasmaChainFn
--------------------------------------------------------------------------------


type PlasmaChainFn = Tagged (SProxy "plasmaChain(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

plasmaChain :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (Tuple4 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
plasmaChain x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: PlasmaChainFn)

--------------------------------------------------------------------------------
-- | PlasmaChainBalanceFn
--------------------------------------------------------------------------------


type PlasmaChainBalanceFn = Tagged (SProxy "plasmaChainBalance()") (Tuple0 )

plasmaChainBalance :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
plasmaChainBalance x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: PlasmaChainBalanceFn)

--------------------------------------------------------------------------------
-- | StartDepositExitFn
--------------------------------------------------------------------------------


type StartDepositExitFn = Tagged (SProxy "startDepositExit(uint256,uint256)") (Tuple2 (Tagged (SProxy "nonce") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "committedFee") (UIntN (D2 :& D5 :& DOne D6))))

startDepositExit :: TransactionOptions MinorUnit -> { nonce :: (UIntN (D2 :& D5 :& DOne D6)), committedFee :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
startDepositExit x0 r = uncurryFields  r $ startDepositExit' x0
   where
    startDepositExit' :: TransactionOptions MinorUnit -> (Tagged (SProxy "nonce") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "committedFee") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    startDepositExit' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: StartDepositExitFn)

--------------------------------------------------------------------------------
-- | StartFeeExitFn
--------------------------------------------------------------------------------


type StartFeeExitFn = Tagged (SProxy "startFeeExit(uint256,uint256)") (Tuple2 (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "committedFee") (UIntN (D2 :& D5 :& DOne D6))))

startFeeExit :: TransactionOptions MinorUnit -> { blockNumber :: (UIntN (D2 :& D5 :& DOne D6)), committedFee :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
startFeeExit x0 r = uncurryFields  r $ startFeeExit' x0
   where
    startFeeExit' :: TransactionOptions MinorUnit -> (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "committedFee") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    startFeeExit' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: StartFeeExitFn)

--------------------------------------------------------------------------------
-- | StartTransactionExitFn
--------------------------------------------------------------------------------


type StartTransactionExitFn = Tagged (SProxy "startTransactionExit(uint256[3],bytes,bytes,bytes,uint256)") (Tuple5 (Tagged (SProxy "txPos") (Vector (DOne D3) (UIntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "txBytes") ByteString) (Tagged (SProxy "proof") ByteString) (Tagged (SProxy "confirmSignatures") ByteString) (Tagged (SProxy "committedFee") (UIntN (D2 :& D5 :& DOne D6))))

startTransactionExit :: TransactionOptions MinorUnit -> { txPos :: (Vector (DOne D3) (UIntN (D2 :& D5 :& DOne D6))), txBytes :: ByteString, proof :: ByteString, confirmSignatures :: ByteString, committedFee :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
startTransactionExit x0 r = uncurryFields  r $ startTransactionExit' x0
   where
    startTransactionExit' :: TransactionOptions MinorUnit -> (Tagged (SProxy "txPos") (Vector (DOne D3) (UIntN (D2 :& D5 :& DOne D6)))) -> (Tagged (SProxy "txBytes") ByteString) -> (Tagged (SProxy "proof") ByteString) -> (Tagged (SProxy "confirmSignatures") ByteString) -> (Tagged (SProxy "committedFee") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    startTransactionExit' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 y1 y2 y3 y4 y5) :: StartTransactionExitFn)

--------------------------------------------------------------------------------
-- | SubmitBlockFn
--------------------------------------------------------------------------------


type SubmitBlockFn = Tagged (SProxy "submitBlock(bytes32[],uint256[],uint256[],uint256)") (Tuple4 (Tagged (SProxy "headers") (Array (BytesN (D3 :& DOne D2)))) (Tagged (SProxy "txnsPerBlock") (Array (UIntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "feePerBlock") (Array (UIntN (D2 :& D5 :& DOne D6)))) (Tagged (SProxy "blockNum") (UIntN (D2 :& D5 :& DOne D6))))

submitBlock :: TransactionOptions NoPay -> { headers :: (Array (BytesN (D3 :& DOne D2))), txnsPerBlock :: (Array (UIntN (D2 :& D5 :& DOne D6))), feePerBlock :: (Array (UIntN (D2 :& D5 :& DOne D6))), blockNum :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
submitBlock x0 r = uncurryFields  r $ submitBlock' x0
   where
    submitBlock' :: TransactionOptions NoPay -> (Tagged (SProxy "headers") (Array (BytesN (D3 :& DOne D2)))) -> (Tagged (SProxy "txnsPerBlock") (Array (UIntN (D2 :& D5 :& DOne D6)))) -> (Tagged (SProxy "feePerBlock") (Array (UIntN (D2 :& D5 :& DOne D6)))) -> (Tagged (SProxy "blockNum") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    submitBlock' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 y1 y2 y3 y4) :: SubmitBlockFn)

--------------------------------------------------------------------------------
-- | TotalWithdrawBalanceFn
--------------------------------------------------------------------------------


type TotalWithdrawBalanceFn = Tagged (SProxy "totalWithdrawBalance()") (Tuple0 )

totalWithdrawBalance :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
totalWithdrawBalance x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TotalWithdrawBalanceFn)

--------------------------------------------------------------------------------
-- | TxExitQueueFn
--------------------------------------------------------------------------------


type TxExitQueueFn = Tagged (SProxy "txExitQueue(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

txExitQueue :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
txExitQueue x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: TxExitQueueFn)

--------------------------------------------------------------------------------
-- | TxExitsFn
--------------------------------------------------------------------------------


type TxExitsFn = Tagged (SProxy "txExits(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

txExits :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (Tuple5 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (DOne D8))))
txExits x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: TxExitsFn)

--------------------------------------------------------------------------------
-- | TxQueueLengthFn
--------------------------------------------------------------------------------


type TxQueueLengthFn = Tagged (SProxy "txQueueLength()") (Tuple0 )

txQueueLength :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
txQueueLength x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: TxQueueLengthFn)

--------------------------------------------------------------------------------
-- | WithdrawFn
--------------------------------------------------------------------------------


type WithdrawFn = Tagged (SProxy "withdraw()") (Tuple0 )

withdraw :: TransactionOptions NoPay -> Web3 HexString
withdraw x0 = sendTx x0 ((tagged $ Tuple0 ) :: WithdrawFn)