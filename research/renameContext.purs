module Perspectives.RenameContext where

import Ace.Types (ACE, Document, DocumentEvent(..))
import Control.Monad.Eff (Eff)
import Data.Array.Partial (head)
import Halogen as H
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextRoleParser (contextDeclaration)
import Perspectives.Syntax (ContextDeclaration(..))
import Prelude (otherwise, ($))

-- Zie: https://stackoverflow.com/questions/44343300/halogen-keyboard-input-example-and-unsubsribing-to-the-events
-- | Creates an `EventSource` for a callback (Query) that accepts one argument.
-- |
-- | - The first argument is the function that attaches the listener.
-- | - The second argument is a handler that optionally produces a value in `f`.
eventSource :: forall f m a eff. MonadAff (avar :: AVAR | eff) m =>
  ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) Unit)
  -> (a -> Maybe (f SubscribeStatus))
  -> EventSource f m

eventSource :: forall f m a eff. MonadAff (avar :: AVAR | eff) m =>
  (Callback a -> Eff (avar :: AVAR | eff) Unit)
  -> MaybeQuery a
  -> EventSource f m


-- | Similar to `eventSource` but allows the attachment function to return an
-- | action to perform when the handler is detached.
eventSource' :: forall f m a eff. MonadAff (avar :: AVAR | eff) m =>
  ((a -> Eff (avar :: AVAR | eff) Unit) -> Eff (avar :: AVAR | eff) (Eff (avar :: AVAR | eff) Unit))
  -> (a -> Maybe (f SubscribeStatus))
  -> EventSource f m

eventSource' :: forall f m a eff. MonadAff (avar :: AVAR | eff) m =>
  (Callback a -> Eff (avar :: AVAR | eff) removeEventListener)
  -> MaybeQuery a
  -> EventSource f m


-- | Creates an `EventSource` for a callback (Query) that accepts no arguments.
-- |
-- | - The first argument is the function that attaches the listener.
-- | - The second argument is the query to raise whenever the listener is
-- |   triggered.
eventSource_ :: forall f m eff. MonadAff (avar :: AVAR | eff) m =>
  (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) Unit)
  -> f SubscribeStatus ->
  EventSource f m

eventSource_ :: forall f m eff. MonadAff (avar :: AVAR | eff) m =>
  (Callback -> Eff (avar :: AVAR | eff) Unit)
  Query
  EventSource f m

-- | Similar to `eventSource_` but allows the attachment function to return an
-- | action to perform when the handler is detached.
eventSource_' :: forall f m eff. MonadAff (avar :: AVAR | eff) m =>
  (Eff (avar :: AVAR | eff) Unit -> Eff (avar :: AVAR | eff) (Eff (avar :: AVAR | eff) Unit))
  -> f SubscribeStatus
  -> EventSource f m

eventSource_' :: forall f m eff. MonadAff (avar :: AVAR | eff) m =>
  (Callback a -> Eff (avar :: AVAR | eff) RemoveEventListener)
  -> Query
  -> EventSource f m

type Callback a = (a -> Eff (avar :: AVAR | eff) Unit)
type MaybeQuery a = (a -> Maybe (f SubscribeStatus))
type RemoveEventListener = (Eff (avar :: AVAR | eff) Unit)
type Query = (f SubscribeStatus)
type Callback = Eff (avar :: AVAR | eff) Unit)

newtype EventSource f m = EventSource (m { producer :: CR.Producer (f SubscribeStatus) m Unit, done :: m Unit })

onChange :: forall eff a.
  Document ->
  (DocumentEvent -> Eff (ace :: ACE | eff) a) ->
  Eff (ace :: ACE | eff) Unit

onChange :: forall eff a.
  Document ->
  (Callback a -> Eff (ace :: ACE | eff) Unit)
