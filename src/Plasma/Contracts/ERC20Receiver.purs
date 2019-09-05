--------------------------------------------------------------------------------
-- | ERC20Receiver
--------------------------------------------------------------------------------

module Plasma.Contracts.ERC20Receiver where

import Prelude 

import Data.Functor.Tagged (Tagged, tagged)
import Data.Symbol (SProxy)
import Network.Ethereum.Web3 (sendTx)
import Network.Ethereum.Web3.Contract.Internal (uncurryFields)
import Network.Ethereum.Web3.Solidity (ByteString, D2, D5, D6, DOne, Tuple3(..), UIntN)
import Network.Ethereum.Web3.Solidity.Size (type (:&))
import Network.Ethereum.Web3.Types (Address, HexString, NoPay, TransactionOptions, Web3)
--------------------------------------------------------------------------------
-- | OnERC20ReceivedFn
--------------------------------------------------------------------------------


type OnERC20ReceivedFn = Tagged (SProxy "onERC20Received(address,uint256,bytes)") (Tuple3 (Tagged (SProxy "_from") Address) (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6))) (Tagged (SProxy "data") ByteString))

onERC20Received :: TransactionOptions NoPay -> { _from :: Address, amount :: (UIntN (D2 :& D5 :& DOne D6)), data :: ByteString } -> Web3 HexString
onERC20Received x0 r = uncurryFields  r $ onERC20Received' x0
   where
    onERC20Received' :: TransactionOptions NoPay -> (Tagged (SProxy "_from") Address) -> (Tagged (SProxy "amount") (UIntN (D2 :& D5 :& DOne D6))) -> (Tagged (SProxy "data") ByteString) -> Web3 HexString
    onERC20Received' y0 y1 y2 y3 = sendTx y0 ((tagged $ Tuple3 y1 y2 y3) :: OnERC20ReceivedFn)