module App.Types where

import Prelude

import App.Env as App
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Either (note')
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Network.Ethereum.Web3 (UIntN, uIntNFromBigNumber, unUIntN)
import Network.Ethereum.Web3.Solidity.Sizes (S256, s256)
import Type.Equality as TE


newtype AppM a = AppM (AppM' Aff a)

type AppM' m a = ReaderT App.Env m a

unAppM :: AppM ~> AppM' Aff
unAppM (AppM s) = s

runAppM :: App.Env -> AppM ~> Aff
runAppM w (AppM ma) = runReaderT ma w

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadRecAppM :: MonadRec AppM
derive newtype instance monadThrow :: MonadThrow Error AppM
derive newtype instance monadError :: MonadError Error AppM
instance monadEffAppM :: MonadEffect AppM where
  liftEffect = AppM <<< liftEffect
instance monadAffAppM :: MonadAff AppM where
  liftAff = AppM <<< liftAff
instance monadAskFoamM :: TE.TypeEquals w App.Env => MonadAsk w AppM where
  ask = AppM (TE.from <$> ask)

newtype Token = Token (UIntN S256)
derive instance newtypeToken :: Newtype Token _
derive instance genericToken :: Generic Token _
instance eqToken :: Eq Token where eq = genericEq
instance ordToken :: Ord Token where compare = genericCompare
instance showToken :: Show Token where show = genericShow

instance encodejsonToken :: EncodeJson Token where
  encodeJson (Token u) = encodeJson $ unUIntN u

instance decodejsonToken :: DecodeJson Token where
  decodeJson = decodeJson >=> \bn -> note'
    (\_ -> "Expected To get valid Token but got: " <> show bn)
      $ Token <$> uIntNFromBigNumber s256 bn
