-- | TODO (sectore / safareli): Extract this module into a library, its currently copied from
-- | https://github.com/f-o-a-m/foam.tcr/blob/master/purs/src/Halogen/Query/EventSource/Extras.purs

module Halogen.Query.EventSource.Extras
  ( eventSourceAff
  , eventSourceAff'
  , eventSourceDOM
  , listening
  ) where

import Prelude

import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Rec.Class (forever)
import Web.Event.EventTarget (EventTarget, addEventListener, eventListener, removeEventListener)
import Web.Event.Event (Event, EventType)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(Just))
import Halogen.Query.EventSource as ES

eventSourceAff
  :: forall f m
   . MonadAff m
  => Aff (Array (f ES.SubscribeStatus))
  -> ES.EventSource f m
eventSourceAff attach = eventSourceAff' \emit -> forever $ attach >>= emit

eventSourceAff'
  :: forall f m. MonadAff m
  => ((Array (f ES.SubscribeStatus) -> Aff Unit) -> Aff Unit)
  -> ES.EventSource f m
eventSourceAff' emitter = ES.eventSource'
  (\emit -> do
    fiber <- launchAff $ emitter $ traverse_ (Just >>> emit >>> liftEffect)
    pure $ launchAff_ $ killFiber (error "EventSource cleanup") fiber
  )
  identity

eventSourceDOM
  :: forall f m
   . MonadAff m
  => EventType
  -> Boolean
  -> EventTarget
  -> ((f ES.SubscribeStatus -> Effect Unit) -> Event -> Effect Unit)
  -> ES.EventSource f m
eventSourceDOM event useCapture target emitter = ES.eventSource'
  (\emit -> do
    listener <- eventListener \e -> emitter emit e
    addEventListener event listener useCapture target
    pure $ removeEventListener event listener useCapture target
  )
  Just

-- TODO (safareli) check how halogen handles multiple events fires with ES.Done
-- does it execute unsubscription action in that instant or pushes to next tick?
listening :: forall f. (Array (ES.SubscribeStatus -> f ES.SubscribeStatus)) -> (Array (f ES.SubscribeStatus))
listening = map \f -> f ES.Listening
