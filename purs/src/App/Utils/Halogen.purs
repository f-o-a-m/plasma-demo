-- | TODO (sectore / safareli): Extract this module into a library, its currently copied from
-- | https://github.com/f-o-a-m/foam.tcr/blob/master/purs/src/App/Utils/Halogen.purs

module App.Utils.Halogen
  ( maybeHtml
  , whenHtml
  , whenClass
  , emptyHTML
  , modifyWithEff
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe, maybe)
import Halogen (get, put)
import Halogen.HTML as HH

maybeHtml :: forall a p i. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeHtml m f = maybe emptyHTML f m

whenHtml :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenHtml test html = if test then html unit else emptyHTML

whenClass :: Boolean -> HH.ClassName -> HH.ClassName
whenClass test class' = if test then class' else HH.ClassName ""


emptyHTML :: forall p i. HH.HTML p i
emptyHTML = HH.text ""

modifyWithEff
  :: forall s m
  .  MonadState s m
  => MonadEffect m
  => (s -> Effect s)
  -> m Unit
modifyWithEff f = do
  s <- get
  s' <- liftEffect $ f s
  put s'
