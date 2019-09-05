--------------------------------------------------------------------------------
-- | SparseMerkleTree
--------------------------------------------------------------------------------

module Plasma.Contracts.SparseMerkleTree where

import Prelude 

import Data.Either (Either)
import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (call, deployContract)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (ByteString, BytesN, D2, D3, D4, D5, D6, DOne, Tuple0(..), Tuple1(..), Tuple3(..), Tuple4(..), UIntN, unTuple1)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (CallError, ChainCursor, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | ConstructorFn
--------------------------------------------------------------------------------


type ConstructorFn = Tagged (SProxy "constructor()") (Tuple0 )

constructor :: TransactionOptions NoPay -> HexString -> Web3 HexString
constructor x0 bc = deployContract x0 bc ((tagged $ Tuple0 ) :: ConstructorFn)

--------------------------------------------------------------------------------
-- | CheckMembershipFn
--------------------------------------------------------------------------------


type CheckMembershipFn = Tagged (SProxy "checkMembership(bytes32,bytes32,uint64,bytes)") (Tuple4 (Tagged (SProxy "leaf") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "root") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "tokenID") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "proof") ByteString))

checkMembership :: TransactionOptions NoPay -> ChainCursor -> { leaf :: (BytesN (D3 :& DOne D2)), root :: (BytesN (D3 :& DOne D2)), tokenID :: (UIntN (D6 :& DOne D4)), proof :: ByteString } -> Web3 (Either CallError Boolean)
checkMembership x0 cm r = uncurryFields  r $ checkMembership' x0 cm
   where
    checkMembership' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "leaf") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "root") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "tokenID") (UIntN (D6 :& DOne D4))) -> (Tagged (SProxy "proof") ByteString) -> Web3 (Either CallError Boolean)
    checkMembership' y0 cm' y2 y3 y4 y5 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple4 y2 y3 y4 y5) :: CheckMembershipFn)

--------------------------------------------------------------------------------
-- | DefaultHashesFn
--------------------------------------------------------------------------------


type DefaultHashesFn = Tagged (SProxy "defaultHashes(uint256)") (Tuple1 (UIntN (D2 :& D5 :& DOne D6)))

defaultHashes :: TransactionOptions NoPay -> ChainCursor -> (UIntN (D2 :& D5 :& DOne D6)) -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
defaultHashes x0 cm x2 = map unTuple1 <$> call x0 cm ((tagged $ Tuple1 x2) :: DefaultHashesFn)

--------------------------------------------------------------------------------
-- | GetRootFn
--------------------------------------------------------------------------------


type GetRootFn = Tagged (SProxy "getRoot(bytes32,uint64,bytes)") (Tuple3 (Tagged (SProxy "leaf") (BytesN (D3 :& DOne D2))) (Tagged (SProxy "index") (UIntN (D6 :& DOne D4))) (Tagged (SProxy "proof") ByteString))

getRoot :: TransactionOptions NoPay -> ChainCursor -> { leaf :: (BytesN (D3 :& DOne D2)), index :: (UIntN (D6 :& DOne D4)), proof :: ByteString } -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
getRoot x0 cm r = uncurryFields  r $ getRoot' x0 cm
   where
    getRoot' :: TransactionOptions NoPay -> ChainCursor -> (Tagged (SProxy "leaf") (BytesN (D3 :& DOne D2))) -> (Tagged (SProxy "index") (UIntN (D6 :& DOne D4))) -> (Tagged (SProxy "proof") ByteString) -> Web3 (Either CallError (BytesN (D3 :& DOne D2)))
    getRoot' y0 cm' y2 y3 y4 = map unTuple1 <$> call y0 cm' ((tagged $ Tuple3 y2 y3 y4) :: GetRootFn)