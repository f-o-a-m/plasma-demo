--------------------------------------------------------------------------------
-- | RootChain
--------------------------------------------------------------------------------

module Plasma.Contracts.RootChain where

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
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D2, D3, D4, D5, D6, D8, DOne, Tuple0(..), Tuple1(..), Tuple2(..), Tuple3(..), Tuple4(..), Tuple5(..), Tuple6(..), Tuple7(..), UIntN, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Network.Ethereum.Web3.Types.TokenUnit (MinorUnit)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor(address)") (Tuple1 (Tagged (SProxy "_vmc") Address))

constructor :: TransactionOptions NoPay -> HexString -> { _vmc :: Address } -> Web3 HexString
constructor x0 bc r = uncurryFields  r $ constructor' x0 bc
   where
    constructor' :: TransactionOptions NoPay -> HexString -> (Tagged (SProxy "_vmc") Address) -> Web3 HexString
    constructor' y0 bc' y2 = deployContract y0 bc' ((tagged $ Tuple1 y2) :: ConstructorFn)



--------------------------------------------------------------------------------
-- | Deposit
--------------------------------------------------------------------------------


newtype Deposit = Deposit {slot :: (UIntN (D6 :& DOne D4)),blockNumber :: (UIntN (D2 :& D5 :& DOne D6)),denomination :: (UIntN (D2 :& D5 :& DOne D6)),from :: Address,contractAddress :: Address}

derive instance newtypeDeposit :: Newtype Deposit _

instance eventFilterDeposit :: EventFilter Deposit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "aa231413cf2b61ec7d73eeef7c722cd8ef6ed7a76e4d9e533867281e94f9a823"),Nothing,Nothing,Nothing]

instance indexedEventDeposit :: IndexedEvent (Tuple3 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "from") Address) (Tagged (SProxy "contractAddress") Address)) (Tuple2 (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "denomination") (UIntN (D2 :& D5 :& DOne D6)))) Deposit where
  isAnonymous _ = false

derive instance genericDeposit :: Generic Deposit _

instance eventGenericDepositShow :: Show Deposit where
	show = genericShow

instance eventGenericDepositeq :: Eq Deposit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SubmittedBlock
--------------------------------------------------------------------------------


newtype SubmittedBlock = SubmittedBlock {blockNumber :: (UIntN (D2 :& D5 :& DOne D6)),root :: (BytesN (D3 :& DOne D2)),timestamp :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSubmittedBlock :: Newtype SubmittedBlock _

instance eventFilterSubmittedBlock :: EventFilter SubmittedBlock where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "6d91cd6ccac8368394df514e6aee19a55264f5ab49a891af91ca86da27bedd4f")]

instance indexedEventSubmittedBlock :: IndexedEvent (Tuple0 ) (Tuple3 (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "root") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "timestamp") (UIntN (D2 :& D5 :& DOne D6)))) SubmittedBlock where
  isAnonymous _ = false

derive instance genericSubmittedBlock :: Generic SubmittedBlock _

instance eventGenericSubmittedBlockShow :: Show SubmittedBlock where
	show = genericShow

instance eventGenericSubmittedBlockeq :: Eq SubmittedBlock where
	eq = genericEq

--------------------------------------------------------------------------------
-- | StartedExit
--------------------------------------------------------------------------------


newtype StartedExit = StartedExit {slot :: (UIntN (D6 :& DOne D4)),owner :: Address}

derive instance newtypeStartedExit :: Newtype StartedExit _

instance eventFilterStartedExit :: EventFilter StartedExit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "baf912dedc1b0ee4647f945b0432e694bce1aa2c4e21052d9776876415874956"),Nothing,Nothing]

instance indexedEventStartedExit :: IndexedEvent (Tuple2 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "owner") Address)) (Tuple0 ) StartedExit where
  isAnonymous _ = false

derive instance genericStartedExit :: Generic StartedExit _

instance eventGenericStartedExitShow :: Show StartedExit where
	show = genericShow

instance eventGenericStartedExiteq :: Eq StartedExit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | ChallengedExit
--------------------------------------------------------------------------------


newtype ChallengedExit = ChallengedExit {slot :: (UIntN (D6 :& DOne D4)),txHash :: (BytesN (D3 :& DOne D2)),challengingBlockNumber :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeChallengedExit :: Newtype ChallengedExit _

instance eventFilterChallengedExit :: EventFilter ChallengedExit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "057d34b2360e71f2764a7189966401bc058621905c0bef7123a6bdfe9a13284b"),Nothing]

instance indexedEventChallengedExit :: IndexedEvent (Tuple1 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4)))) (Tuple2 (Tagged (SProxy "txHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "challengingBlockNumber") (UIntN (D2 :& D5 :& DOne D6)))) ChallengedExit where
  isAnonymous _ = false

derive instance genericChallengedExit :: Generic ChallengedExit _

instance eventGenericChallengedExitShow :: Show ChallengedExit where
	show = genericShow

instance eventGenericChallengedExiteq :: Eq ChallengedExit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | RespondedExitChallenge
--------------------------------------------------------------------------------


newtype RespondedExitChallenge = RespondedExitChallenge {slot :: (UIntN (D6 :& DOne D4))}

derive instance newtypeRespondedExitChallenge :: Newtype RespondedExitChallenge _

instance eventFilterRespondedExitChallenge :: EventFilter RespondedExitChallenge where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "7deeb80e1644537d3ba4f917cfcbaba62b54354e9f2598d6885d52e359885e25"),Nothing]

instance indexedEventRespondedExitChallenge :: IndexedEvent (Tuple1 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4)))) (Tuple0 ) RespondedExitChallenge where
  isAnonymous _ = false

derive instance genericRespondedExitChallenge :: Generic RespondedExitChallenge _

instance eventGenericRespondedExitChallengeShow :: Show RespondedExitChallenge where
	show = genericShow

instance eventGenericRespondedExitChallengeeq :: Eq RespondedExitChallenge where
	eq = genericEq

--------------------------------------------------------------------------------
-- | CoinReset
--------------------------------------------------------------------------------


newtype CoinReset = CoinReset {slot :: (UIntN (D6 :& DOne D4)),owner :: Address}

derive instance newtypeCoinReset :: Newtype CoinReset _

instance eventFilterCoinReset :: EventFilter CoinReset where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "35cc9c4d0d9a2ce748d131382582af17dbf1f0d72167ea7906ea3f18597b31a4"),Nothing,Nothing]

instance indexedEventCoinReset :: IndexedEvent (Tuple2 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "owner") Address)) (Tuple0 ) CoinReset where
  isAnonymous _ = false

derive instance genericCoinReset :: Generic CoinReset _

instance eventGenericCoinResetShow :: Show CoinReset where
	show = genericShow

instance eventGenericCoinReseteq :: Eq CoinReset where
	eq = genericEq

--------------------------------------------------------------------------------
-- | FinalizedExit
--------------------------------------------------------------------------------


newtype FinalizedExit = FinalizedExit {slot :: (UIntN (D6 :& DOne D4)),owner :: Address}

derive instance newtypeFinalizedExit :: Newtype FinalizedExit _

instance eventFilterFinalizedExit :: EventFilter FinalizedExit where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "432647a5fb9bdea356d78f8e3d83b6ddc2e78b4e4a702ac7eb968f7fe03d9dda"),Nothing]

instance indexedEventFinalizedExit :: IndexedEvent (Tuple1 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4)))) (Tuple1 (Tagged (SProxy "owner") Address)) FinalizedExit where
  isAnonymous _ = false

derive instance genericFinalizedExit :: Generic FinalizedExit _

instance eventGenericFinalizedExitShow :: Show FinalizedExit where
	show = genericShow

instance eventGenericFinalizedExiteq :: Eq FinalizedExit where
	eq = genericEq

--------------------------------------------------------------------------------
-- | FreedBond
--------------------------------------------------------------------------------


newtype FreedBond = FreedBond {from :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeFreedBond :: Newtype FreedBond _

instance eventFilterFreedBond :: EventFilter FreedBond where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "5406cdbb33189b887da81a90c7ff307f3af1854b2504d0bde438b8bb5b7d8b52"),Nothing]

instance indexedEventFreedBond :: IndexedEvent (Tuple1 (Tagged (SProxy "from") Address)) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) FreedBond where
  isAnonymous _ = false

derive instance genericFreedBond :: Generic FreedBond _

instance eventGenericFreedBondShow :: Show FreedBond where
	show = genericShow

instance eventGenericFreedBondeq :: Eq FreedBond where
	eq = genericEq

--------------------------------------------------------------------------------
-- | SlashedBond
--------------------------------------------------------------------------------


newtype SlashedBond = SlashedBond {from :: Address,to :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeSlashedBond :: Newtype SlashedBond _

instance eventFilterSlashedBond :: EventFilter SlashedBond where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "bd6bf2f753dc1a7dee94a7abd7645adc6c228c2af17fd72d9104674cc5d21273"),Nothing,Nothing]

instance indexedEventSlashedBond :: IndexedEvent (Tuple2 (Tagged (SProxy "from") Address) (Tagged (SProxy "to") Address)) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) SlashedBond where
  isAnonymous _ = false

derive instance genericSlashedBond :: Generic SlashedBond _

instance eventGenericSlashedBondShow :: Show SlashedBond where
	show = genericShow

instance eventGenericSlashedBondeq :: Eq SlashedBond where
	eq = genericEq

--------------------------------------------------------------------------------
-- | WithdrewBonds
--------------------------------------------------------------------------------


newtype WithdrewBonds = WithdrewBonds {from :: Address,amount :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeWithdrewBonds :: Newtype WithdrewBonds _

instance eventFilterWithdrewBonds :: EventFilter WithdrewBonds where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "ba49a755e2e4831fb6ef34b384178b03af45679532c9ac5a2199b27b4dddea8d"),Nothing]

instance indexedEventWithdrewBonds :: IndexedEvent (Tuple1 (Tagged (SProxy "from") Address)) (Tuple1 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6)))) WithdrewBonds where
  isAnonymous _ = false

derive instance genericWithdrewBonds :: Generic WithdrewBonds _

instance eventGenericWithdrewBondsShow :: Show WithdrewBonds where
	show = genericShow

instance eventGenericWithdrewBondseq :: Eq WithdrewBonds where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Withdrew
--------------------------------------------------------------------------------


newtype Withdrew = Withdrew {owner :: Address,slot :: (UIntN (D6 :& DOne D4)),mode :: (UIntN (DOne D8)),contractAddress :: Address,uid :: (UIntN (D2 :& D5 :& DOne D6)),denomination :: (UIntN (D2 :& D5 :& DOne D6))}

derive instance newtypeWithdrew :: Newtype Withdrew _

instance eventFilterWithdrew :: EventFilter Withdrew where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "c753e3ccb1413ef7152e8d33fd0abfb140e236ce17bf9ae40a2a6940bcb7ed7b"),Nothing,Nothing]

instance indexedEventWithdrew :: IndexedEvent (Tuple2 (Tagged (SProxy "owner") Address) (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4)))) (Tuple4 (Tagged (SProxy "mode") (UIntN (DOne D8))) (Tagged (SProxy "contractAddress") Address) (Tagged (SProxy "uid") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "denomination") (UIntN (D2 :& D5 :& DOne D6)))) Withdrew where
  isAnonymous _ = false

derive instance genericWithdrew :: Generic Withdrew _

instance eventGenericWithdrewShow :: Show Withdrew where
	show = genericShow

instance eventGenericWithdreweq :: Eq Withdrew where
	eq = genericEq

--------------------------------------------------------------------------------
-- | Paused
--------------------------------------------------------------------------------


newtype Paused = Paused {status :: Boolean}

derive instance newtypePaused :: Newtype Paused _

instance eventFilterPaused :: EventFilter Paused where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "0e2fb031ee032dc02d8011dc50b816eb450cf856abd8261680dac74f72165bd2")]

instance indexedEventPaused :: IndexedEvent (Tuple0 ) (Tuple1 (Tagged (SProxy "status") Boolean)) Paused where
  isAnonymous _ = false

derive instance genericPaused :: Generic Paused _

instance eventGenericPausedShow :: Show Paused where
	show = genericShow

instance eventGenericPausedeq :: Eq Paused where
	eq = genericEq

--------------------------------------------------------------------------------
-- | BalancesFn
--------------------------------------------------------------------------------


type BalancesFn = Tagged (SProxy "balances(address)") (Tuple1 Address)

balances :: TransactionOptions NoPay -> ChainCursor -> Address -> Web3 (Either CallError (Tuple2 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6))))
balances x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: BalancesFn)

--------------------------------------------------------------------------------
-- | CancelExitFn
--------------------------------------------------------------------------------


type CancelExitFn = Tagged (SProxy "cancelExit(uint64)") (Tuple1 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))))

cancelExit :: TransactionOptions NoPay -> { slot :: (UIntN (D6 :& DOne D4)) } -> Web3 HexString
cancelExit x0 r = uncurryFields  r $ cancelExit' x0
   where
    cancelExit' :: TransactionOptions NoPay -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> Web3 HexString
    cancelExit' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: CancelExitFn)

--------------------------------------------------------------------------------
-- | CancelExitsFn
--------------------------------------------------------------------------------


type CancelExitsFn = Tagged (SProxy "cancelExits(uint64[])") (Tuple1 (Tagged (SProxy "slots") (Array (UIntN (D6 :& DOne D4)))))

cancelExits :: TransactionOptions NoPay -> { slots :: (Array (UIntN (D6 :& DOne D4))) } -> Web3 HexString
cancelExits x0 r = uncurryFields  r $ cancelExits' x0
   where
    cancelExits' :: TransactionOptions NoPay -> (Tagged (SProxy "slots") (Array (UIntN (D6 :& DOne D4)))) -> Web3 HexString
    cancelExits' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: CancelExitsFn)

--------------------------------------------------------------------------------
-- | ChallengeAfterFn
--------------------------------------------------------------------------------


type ChallengeAfterFn = Tagged (SProxy "challengeAfter(uint64,uint256,bytes,bytes,bytes)") (Tuple5 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "challengingBlockNumber") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "challengingTransaction") ByteString) (Tagged (SProxy "proof") ByteString) (Tagged (SProxy "signature") ByteString))

challengeAfter :: TransactionOptions NoPay -> { slot :: (UIntN (D6 :& DOne D4)), challengingBlockNumber :: (UIntN (D2 :& D5 :& DOne D6)), challengingTransaction :: ByteString, proof :: ByteString, signature :: ByteString } -> Web3 HexString
challengeAfter x0 r = uncurryFields  r $ challengeAfter' x0
   where
    challengeAfter' :: TransactionOptions NoPay -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> (Tagged (SProxy "challengingBlockNumber") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "challengingTransaction") ByteString) -> (Tagged (SProxy "proof") ByteString) -> (Tagged (SProxy "signature") ByteString) -> Web3 HexString
    challengeAfter' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 y1 y2 y3 y4 y5) :: ChallengeAfterFn)

--------------------------------------------------------------------------------
-- | ChallengeBeforeFn
--------------------------------------------------------------------------------


type ChallengeBeforeFn = Tagged (SProxy "challengeBefore(uint64,bytes,bytes,bytes,uint256)") (Tuple5 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "txBytes") ByteString) (Tagged (SProxy "txInclusionProof") ByteString) (Tagged (SProxy "signature") ByteString) (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))))

challengeBefore :: TransactionOptions MinorUnit -> { slot :: (UIntN (D6 :& DOne D4)), txBytes :: ByteString, txInclusionProof :: ByteString, signature :: ByteString, blockNumber :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 HexString
challengeBefore x0 r = uncurryFields  r $ challengeBefore' x0
   where
    challengeBefore' :: TransactionOptions MinorUnit -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> (Tagged (SProxy "txBytes") ByteString) -> (Tagged (SProxy "txInclusionProof") ByteString) -> (Tagged (SProxy "signature") ByteString) -> (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 HexString
    challengeBefore' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 y1 y2 y3 y4 y5) :: ChallengeBeforeFn)

--------------------------------------------------------------------------------
-- | ChallengeBetweenFn
--------------------------------------------------------------------------------


type ChallengeBetweenFn = Tagged (SProxy "challengeBetween(uint64,uint256,bytes,bytes,bytes)") (Tuple5 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "challengingBlockNumber") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "challengingTransaction") ByteString) (Tagged (SProxy "proof") ByteString) (Tagged (SProxy "signature") ByteString))

challengeBetween :: TransactionOptions NoPay -> { slot :: (UIntN (D6 :& DOne D4)), challengingBlockNumber :: (UIntN (D2 :& D5 :& DOne D6)), challengingTransaction :: ByteString, proof :: ByteString, signature :: ByteString } -> Web3 HexString
challengeBetween x0 r = uncurryFields  r $ challengeBetween' x0
   where
    challengeBetween' :: TransactionOptions NoPay -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> (Tagged (SProxy "challengingBlockNumber") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "challengingTransaction") ByteString) -> (Tagged (SProxy "proof") ByteString) -> (Tagged (SProxy "signature") ByteString) -> Web3 HexString
    challengeBetween' y0 y1 y2 y3 y4 y5 = sendTx y0 ((tagged $ Tuple5 y1 y2 y3 y4 y5) :: ChallengeBetweenFn)

--------------------------------------------------------------------------------
-- | CheckMembershipFn
--------------------------------------------------------------------------------


type CheckMembershipFn = Tagged (SProxy "checkMembership(bytes32,bytes32,uint64,bytes)") (Tuple4 (Tagged (SProxy "txHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "root") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "proof") ByteString))

checkMembership :: TransactionOptions NoPay -> ChainCursor -> { txHash :: (BytesN (D3 :& DOne D2)), root :: (BytesN (D3 :& DOne D2)), slot :: (UIntN (D6 :& DOne D4)), proof :: ByteString } -> Web3 (Either CallError Boolean)
checkMembership x0 cm r = uncurryFields  r $ checkMembership' x0 cm
   where
    checkMembership' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "txHash") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "root") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> (Tagged (SProxy "proof") ByteString) -> Web3 (Either CallError Boolean)
    checkMembership' y0 cm' y2 y3 y4 y5 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple4 y2 y3 y4 y5) :: CheckMembershipFn)

--------------------------------------------------------------------------------
-- | ChildBlockIntervalFn
--------------------------------------------------------------------------------


type ChildBlockIntervalFn = Tagged (SProxy "childBlockInterval()") (Tuple0 )

childBlockInterval :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
childBlockInterval x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: ChildBlockIntervalFn)

--------------------------------------------------------------------------------
-- | ChildChainFn
--------------------------------------------------------------------------------


type ChildChainFn = Tagged (SProxy "childChain(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

childChain :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (Tuple2 (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
childChain x0 cm x2 = call x0 cm ((tagged $ Tuple1 x2) :: ChildChainFn)

--------------------------------------------------------------------------------
-- | CurrentBlockFn
--------------------------------------------------------------------------------


type CurrentBlockFn = Tagged (SProxy "currentBlock()") (Tuple0 )

currentBlock :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D2 :& D5 :& DOne D6)))
currentBlock x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: CurrentBlockFn)

--------------------------------------------------------------------------------
-- | DepositERC20Fn
--------------------------------------------------------------------------------


type DepositERC20Fn = Tagged (SProxy "depositERC20(uint256,address)") (Tuple2 (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "contractAddress") Address))

depositERC20 :: TransactionOptions NoPay -> { amount :: (UIntN (D2 :& D5 :& DOne D6)), contractAddress :: Address } -> Web3 HexString
depositERC20 x0 r = uncurryFields  r $ depositERC20' x0
   where
    depositERC20' :: TransactionOptions NoPay -> (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "contractAddress") Address) -> Web3 HexString
    depositERC20' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: DepositERC20Fn)

--------------------------------------------------------------------------------
-- | DepositERC721Fn
--------------------------------------------------------------------------------


type DepositERC721Fn = Tagged (SProxy "depositERC721(uint256,address)") (Tuple2 (Tagged (SProxy "uid") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "contractAddress") Address))

depositERC721 :: TransactionOptions NoPay -> { uid :: (UIntN (D2 :& D5 :& DOne D6)), contractAddress :: Address } -> Web3 HexString
depositERC721 x0 r = uncurryFields  r $ depositERC721' x0
   where
    depositERC721' :: TransactionOptions NoPay -> (Tagged (SProxy "uid") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "contractAddress") Address) -> Web3 HexString
    depositERC721' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: DepositERC721Fn)

--------------------------------------------------------------------------------
-- | FinalizeExitFn
--------------------------------------------------------------------------------


type FinalizeExitFn = Tagged (SProxy "finalizeExit(uint64)") (Tuple1 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))))

finalizeExit :: TransactionOptions NoPay -> { slot :: (UIntN (D6 :& DOne D4)) } -> Web3 HexString
finalizeExit x0 r = uncurryFields  r $ finalizeExit' x0
   where
    finalizeExit' :: TransactionOptions NoPay -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> Web3 HexString
    finalizeExit' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: FinalizeExitFn)

--------------------------------------------------------------------------------
-- | FinalizeExitsFn
--------------------------------------------------------------------------------


type FinalizeExitsFn = Tagged (SProxy "finalizeExits(uint64[])") (Tuple1 (Tagged (SProxy "slots") (Array (UIntN (D6 :& DOne D4)))))

finalizeExits :: TransactionOptions NoPay -> { slots :: (Array (UIntN (D6 :& DOne D4))) } -> Web3 HexString
finalizeExits x0 r = uncurryFields  r $ finalizeExits' x0
   where
    finalizeExits' :: TransactionOptions NoPay -> (Tagged (SProxy "slots") (Array (UIntN (D6 :& DOne D4)))) -> Web3 HexString
    finalizeExits' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: FinalizeExitsFn)

--------------------------------------------------------------------------------
-- | GetBlockRootFn
--------------------------------------------------------------------------------


type GetBlockRootFn = Tagged (SProxy "getBlockRoot(uint256)") (Tuple1 (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))))

getBlockRoot :: TransactionOptions NoPay -> ChainCursor -> { blockNumber :: (UIntN (D2 :& D5 :& DOne D6)) } -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
getBlockRoot x0 cm r = uncurryFields  r $ getBlockRoot' x0 cm
   where
    getBlockRoot' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))) -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
    getBlockRoot' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: GetBlockRootFn)

--------------------------------------------------------------------------------
-- | GetChallengeFn
--------------------------------------------------------------------------------


type GetChallengeFn = Tagged (SProxy "getChallenge(uint64,bytes32)") (Tuple2 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "txHash") (BytesN (D3 :& DOne D2))))

getChallenge :: TransactionOptions NoPay -> ChainCursor -> { slot :: (UIntN (D6 :& DOne D4)), txHash :: (BytesN (D3 :& DOne D2)) } -> Web3 (Either CallError (Tuple4 Address Address (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
getChallenge x0 cm r = uncurryFields  r $ getChallenge' x0 cm
   where
    getChallenge' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> (Tagged (SProxy "txHash") (BytesN (D3 :& DOne D2))) -> Web3 (Either CallError (Tuple4 Address Address (BytesN (D3 :& DOne D2)) (UIntN (D2 :& D5 :& DOne D6))))
    getChallenge' y0 cm' y2 y3 = call y0 cm' ((tagged $ Tuple2 y2 y3) :: GetChallengeFn)

--------------------------------------------------------------------------------
-- | GetExitFn
--------------------------------------------------------------------------------


type GetExitFn = Tagged (SProxy "getExit(uint64)") (Tuple1 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))))

getExit :: TransactionOptions NoPay -> ChainCursor -> { slot :: (UIntN (D6 :& DOne D4)) } -> Web3 (Either CallError (Tuple5 Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (DOne D8)) (UIntN (D2 :& D5 :& DOne D6))))
getExit x0 cm r = uncurryFields  r $ getExit' x0 cm
   where
    getExit' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> Web3 (Either CallError (Tuple5 Address (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (DOne D8)) (UIntN (D2 :& D5 :& DOne D6))))
    getExit' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 y2) :: GetExitFn)

--------------------------------------------------------------------------------
-- | GetPlasmaCoinFn
--------------------------------------------------------------------------------


type GetPlasmaCoinFn = Tagged (SProxy "getPlasmaCoin(uint64)") (Tuple1 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))))

getPlasmaCoin :: TransactionOptions NoPay -> ChainCursor -> { slot :: (UIntN (D6 :& DOne D4)) } -> Web3 (Either CallError (Tuple7 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (DOne D8)) (UIntN (DOne D8)) Address))
getPlasmaCoin x0 cm r = uncurryFields  r $ getPlasmaCoin' x0 cm
   where
    getPlasmaCoin' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> Web3 (Either CallError (Tuple7 (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) (UIntN (D2 :& D5 :& DOne D6)) Address (UIntN (DOne D8)) (UIntN (DOne D8)) Address))
    getPlasmaCoin' y0 cm' y2 = call y0 cm' ((tagged $ Tuple1 y2) :: GetPlasmaCoinFn)

--------------------------------------------------------------------------------
-- | NumCoinsFn
--------------------------------------------------------------------------------


type NumCoinsFn = Tagged (SProxy "numCoins()") (Tuple0 )

numCoins :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError (UIntN (D6 :& DOne D4)))
numCoins x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: NumCoinsFn)

--------------------------------------------------------------------------------
-- | OnERC20ReceivedFn
--------------------------------------------------------------------------------


type OnERC20ReceivedFn = Tagged (SProxy "onERC20Received(address,uint256,bytes)") (Tuple3 Address (UIntN (D2 :& D5 :& DOne D6)) ByteString)

onERC20Received :: TransactionOptions NoPay -> Address -> (UIntN (D2 :& D5 :& DOne D6)) -> ByteString -> Web3 HexString
onERC20Received x0 x1 x2 x3 = sendTx x0 ((tagged $ Tuple3 x1 x2 x3) :: OnERC20ReceivedFn)

--------------------------------------------------------------------------------
-- | OnERC721Received3Fn
--------------------------------------------------------------------------------


type OnERC721Received3Fn = Tagged (SProxy "onERC721Received3(address,uint256,bytes)") (Tuple3 Address (UIntN (D2 :& D5 :& DOne D6)) ByteString)

onERC721Received3 :: TransactionOptions NoPay -> Address -> (UIntN (D2 :& D5 :& DOne D6)) -> ByteString -> Web3 HexString
onERC721Received3 x0 x1 x2 x3 = sendTx x0 ((tagged $ Tuple3 x1 x2 x3) :: OnERC721Received3Fn)

--------------------------------------------------------------------------------
-- | OnERC721Received4Fn
--------------------------------------------------------------------------------


type OnERC721Received4Fn = Tagged (SProxy "onERC721Received4(address,address,uint256,bytes)") (Tuple4 (Tagged (SProxy "_operator") Address) (Tagged (SProxy "_from") Address) (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "_data") ByteString))

onERC721Received4 :: TransactionOptions NoPay -> { _operator :: Address, _from :: Address, _tokenId :: (UIntN (D2 :& D5 :& DOne D6)), _data :: ByteString } -> Web3 HexString
onERC721Received4 x0 r = uncurryFields  r $ onERC721Received4' x0
   where
    onERC721Received4' :: TransactionOptions NoPay -> (Tagged (SProxy "_operator") Address) -> (Tagged (SProxy "_from") Address) -> (Tagged (SProxy "_tokenId") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "_data") ByteString) -> Web3 HexString
    onERC721Received4' y0 y1 y2 y3 y4 = sendTx y0 ((tagged $ Tuple4 y1 y2 y3 y4) :: OnERC721Received4Fn)

--------------------------------------------------------------------------------
-- | PauseFn
--------------------------------------------------------------------------------


type PauseFn = Tagged (SProxy "pause()") (Tuple0 )

pause :: TransactionOptions NoPay -> Web3 HexString
pause x0 = sendTx x0 ((tagged $ Tuple0 ) :: PauseFn)

--------------------------------------------------------------------------------
-- | RespondChallengeBeforeFn
--------------------------------------------------------------------------------


type RespondChallengeBeforeFn = Tagged (SProxy "respondChallengeBefore(uint64,bytes32,uint256,bytes,bytes,bytes)") (Tuple6 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "challengingTxHash") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "respondingBlockNumber") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "respondingTransaction") ByteString) (Tagged (SProxy "proof") ByteString) (Tagged (SProxy "signature") ByteString))

respondChallengeBefore :: TransactionOptions NoPay -> { slot :: (UIntN (D6 :& DOne D4)), challengingTxHash :: (BytesN (D3 :& DOne D2)), respondingBlockNumber :: (UIntN (D2 :& D5 :& DOne D6)), respondingTransaction :: ByteString, proof :: ByteString, signature :: ByteString } -> Web3 HexString
respondChallengeBefore x0 r = uncurryFields  r $ respondChallengeBefore' x0
   where
    respondChallengeBefore' :: TransactionOptions NoPay -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> (Tagged (SProxy "challengingTxHash") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "respondingBlockNumber") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "respondingTransaction") ByteString) -> (Tagged (SProxy "proof") ByteString) -> (Tagged (SProxy "signature") ByteString) -> Web3 HexString
    respondChallengeBefore' y0 y1 y2 y3 y4 y5 y6 = sendTx y0 ((tagged $ Tuple6 y1 y2 y3 y4 y5 y6) :: RespondChallengeBeforeFn)

--------------------------------------------------------------------------------
-- | StartExitFn
--------------------------------------------------------------------------------


type StartExitFn = Tagged (SProxy "startExit(uint64,bytes,bytes,bytes,bytes,bytes,uint256[2])") (Tuple7 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "prevTxBytes") ByteString) (Tagged (SProxy "exitingTxBytes") ByteString) (Tagged (SProxy "prevTxInclusionProof") ByteString) (Tagged (SProxy "exitingTxInclusionProof") ByteString) (Tagged (SProxy "signature") ByteString) (Tagged (SProxy "blocks") (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6)))))

startExit :: TransactionOptions MinorUnit -> { slot :: (UIntN (D6 :& DOne D4)), prevTxBytes :: ByteString, exitingTxBytes :: ByteString, prevTxInclusionProof :: ByteString, exitingTxInclusionProof :: ByteString, signature :: ByteString, blocks :: (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6))) } -> Web3 HexString
startExit x0 r = uncurryFields  r $ startExit' x0
   where
    startExit' :: TransactionOptions MinorUnit -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> (Tagged (SProxy "prevTxBytes") ByteString) -> (Tagged (SProxy "exitingTxBytes") ByteString) -> (Tagged (SProxy "prevTxInclusionProof") ByteString) -> (Tagged (SProxy "exitingTxInclusionProof") ByteString) -> (Tagged (SProxy "signature") ByteString) -> (Tagged (SProxy "blocks") (Vector (DOne D2) (UIntN (D2 :& D5 :& DOne D6)))) -> Web3 HexString
    startExit' y0 y1 y2 y3 y4 y5 y6 y7 = sendTx y0 ((tagged $ Tuple7 y1 y2 y3 y4 y5 y6 y7) :: StartExitFn)

--------------------------------------------------------------------------------
-- | SubmitBlockFn
--------------------------------------------------------------------------------


type SubmitBlockFn = Tagged (SProxy "submitBlock(uint256,bytes32)") (Tuple2 (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "root") (BytesN (D3 :& DOne D2))))

submitBlock :: TransactionOptions NoPay -> { blockNumber :: (UIntN (D2 :& D5 :& DOne D6)), root :: (BytesN (D3 :& DOne D2)) } -> Web3 HexString
submitBlock x0 r = uncurryFields  r $ submitBlock' x0
   where
    submitBlock' :: TransactionOptions NoPay -> (Tagged (SProxy "blockNumber") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "root") (BytesN (D3 :& DOne D2))) -> Web3 HexString
    submitBlock' y0 y1 y2 = sendTx y0 ((tagged $ Tuple2 y1 y2) :: SubmitBlockFn)

--------------------------------------------------------------------------------
-- | UnpauseFn
--------------------------------------------------------------------------------


type UnpauseFn = Tagged (SProxy "unpause()") (Tuple0 )

unpause :: TransactionOptions NoPay -> Web3 HexString
unpause x0 = sendTx x0 ((tagged $ Tuple0 ) :: UnpauseFn)

--------------------------------------------------------------------------------
-- | WithdrawFn
--------------------------------------------------------------------------------


type WithdrawFn = Tagged (SProxy "withdraw(uint64)") (Tuple1 (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))))

withdraw :: TransactionOptions NoPay -> { slot :: (UIntN (D6 :& DOne D4)) } -> Web3 HexString
withdraw x0 r = uncurryFields  r $ withdraw' x0
   where
    withdraw' :: TransactionOptions NoPay -> (Tagged (SProxy "slot") (UIntN (D6 :& DOne D4))) -> Web3 HexString
    withdraw' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: WithdrawFn)

--------------------------------------------------------------------------------
-- | WithdrawBondsFn
--------------------------------------------------------------------------------


type WithdrawBondsFn = Tagged (SProxy "withdrawBonds()") (Tuple0 )

withdrawBonds :: TransactionOptions NoPay -> Web3 HexString
withdrawBonds x0 = sendTx x0 ((tagged $ Tuple0 ) :: WithdrawBondsFn)