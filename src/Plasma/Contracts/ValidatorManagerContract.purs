--------------------------------------------------------------------------------
-- | ValidatorManagerContract
--------------------------------------------------------------------------------

module Plasma.Contracts.ValidatorManagerContract where

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
import Network.Ethereum.Web3 (_address, _topics, call, class EventFilter, deployContract, sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (Tuple0(..), Tuple1(..), Tuple2, class IndexedEvent, unTuple1)
import Network.Ethereum.Web3.Types (Address, CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3, defaultFilter, mkHexString)
import Partial.Unsafe (unsafePartial)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | OwnershipRenounced
--------------------------------------------------------------------------------


newtype OwnershipRenounced = OwnershipRenounced {previousOwner :: Address}

derive instance newtypeOwnershipRenounced :: Newtype OwnershipRenounced _

instance eventFilterOwnershipRenounced :: EventFilter OwnershipRenounced where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "f8df31144d9c2f0f6b59d69b8b98abd5459d07f2742c4df920b25aae33c64820"),Nothing]

instance indexedEventOwnershipRenounced :: IndexedEvent (Tuple1 (Tagged (SProxy "previousOwner") Address)) (Tuple0 ) OwnershipRenounced where
  isAnonymous _ = false

derive instance genericOwnershipRenounced :: Generic OwnershipRenounced _

instance eventGenericOwnershipRenouncedShow :: Show OwnershipRenounced where
	show = genericShow

instance eventGenericOwnershipRenouncedeq :: Eq OwnershipRenounced where
	eq = genericEq

--------------------------------------------------------------------------------
-- | OwnershipTransferred
--------------------------------------------------------------------------------


newtype OwnershipTransferred = OwnershipTransferred {previousOwner :: Address,newOwner :: Address}

derive instance newtypeOwnershipTransferred :: Newtype OwnershipTransferred _

instance eventFilterOwnershipTransferred :: EventFilter OwnershipTransferred where
	eventFilter _ addr = defaultFilter
		# _address .~ Just addr
		# _topics .~ Just [Just ( unsafePartial $ fromJust $ mkHexString "8be0079c531659141344cd1fd0a4f28419497f9722a3daafe3b4186f6b6457e0"),Nothing,Nothing]

instance indexedEventOwnershipTransferred :: IndexedEvent (Tuple2 (Tagged (SProxy "previousOwner") Address) (Tagged (SProxy "newOwner") Address)) (Tuple0 ) OwnershipTransferred where
  isAnonymous _ = false

derive instance genericOwnershipTransferred :: Generic OwnershipTransferred _

instance eventGenericOwnershipTransferredShow :: Show OwnershipTransferred where
	show = genericShow

instance eventGenericOwnershipTransferredeq :: Eq OwnershipTransferred where
	eq = genericEq

--------------------------------------------------------------------------------
-- | AllowedTokensFn
--------------------------------------------------------------------------------


type AllowedTokensFn = Tagged (SProxy "allowedTokens(address)") (Tuple1 Address)

allowedTokens :: TransactionOptions NoPay -> ChainCursor -> Address -> Web3 (Either CallError Boolean)
allowedTokens x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: AllowedTokensFn)

--------------------------------------------------------------------------------
-- | CheckValidatorFn
--------------------------------------------------------------------------------


type CheckValidatorFn = Tagged (SProxy "checkValidator(address)") (Tuple1 (Tagged (SProxy "_address") Address))

checkValidator :: TransactionOptions NoPay -> ChainCursor -> { _address :: Address } -> Web3 (Either CallError Boolean)
checkValidator x0 cm r = uncurryFields  r $ checkValidator' x0 cm
   where
    checkValidator' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "_address") Address) -> Web3 (Either CallError Boolean)
    checkValidator' y0 cm' y2 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple1 y2) :: CheckValidatorFn)

--------------------------------------------------------------------------------
-- | OwnerFn
--------------------------------------------------------------------------------


type OwnerFn = Tagged (SProxy "owner()") (Tuple0 )

owner :: TransactionOptions NoPay -> ChainCursor -> Web3 (Either CallError Address)
owner x0 cm = map unTuple1 <$> call x0 cm ((tagged $ Tuple0 ) :: OwnerFn)

--------------------------------------------------------------------------------
-- | RenounceOwnershipFn
--------------------------------------------------------------------------------


type RenounceOwnershipFn = Tagged (SProxy "renounceOwnership()") (Tuple0 )

renounceOwnership :: TransactionOptions NoPay -> Web3 HexString
renounceOwnership x0 = sendTx x0 ((tagged $ Tuple0 ) :: RenounceOwnershipFn)

--------------------------------------------------------------------------------
-- | ToggleTokenFn
--------------------------------------------------------------------------------


type ToggleTokenFn = Tagged (SProxy "toggleToken(address)") (Tuple1 (Tagged (SProxy "_token") Address))

toggleToken :: TransactionOptions NoPay -> { _token :: Address } -> Web3 HexString
toggleToken x0 r = uncurryFields  r $ toggleToken' x0
   where
    toggleToken' :: TransactionOptions NoPay -> (Tagged (SProxy "_token") Address) -> Web3 HexString
    toggleToken' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: ToggleTokenFn)

--------------------------------------------------------------------------------
-- | ToggleValidatorFn
--------------------------------------------------------------------------------


type ToggleValidatorFn = Tagged (SProxy "toggleValidator(address)") (Tuple1 (Tagged (SProxy "_address") Address))

toggleValidator :: TransactionOptions NoPay -> { _address :: Address } -> Web3 HexString
toggleValidator x0 r = uncurryFields  r $ toggleValidator' x0
   where
    toggleValidator' :: TransactionOptions NoPay -> (Tagged (SProxy "_address") Address) -> Web3 HexString
    toggleValidator' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: ToggleValidatorFn)

--------------------------------------------------------------------------------
-- | TransferOwnershipFn
--------------------------------------------------------------------------------


type TransferOwnershipFn = Tagged (SProxy "transferOwnership(address)") (Tuple1 (Tagged (SProxy "_newOwner") Address))

transferOwnership :: TransactionOptions NoPay -> { _newOwner :: Address } -> Web3 HexString
transferOwnership x0 r = uncurryFields  r $ transferOwnership' x0
   where
    transferOwnership' :: TransactionOptions NoPay -> (Tagged (SProxy "_newOwner") Address) -> Web3 HexString
    transferOwnership' y0 y1 = sendTx y0 ((tagged $ Tuple1 y1) :: TransferOwnershipFn)

--------------------------------------------------------------------------------
-- | ValidatorsFn
--------------------------------------------------------------------------------


type ValidatorsFn = Tagged (SProxy "validators(address)") (Tuple1 Address)

validators :: TransactionOptions NoPay -> ChainCursor -> Address -> Web3 (Either CallError Boolean)
validators x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: ValidatorsFn)