module Perspectives.TripleGetters.TrackedAs where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Perspectives.CoreTypes (Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), MonadPerspectivesQuery, type (~~>), type (**>))
import Perspectives.EntiteitAndRDFAliases (Predicate)
import Perspectives.TripleAdministration (addToTripleIndex, detectCycles, lookupInTripleIndex, memorize, memorizeQueryResults)
import Prelude (bind, flip, pure, ($), (<>), discard)
import Unsafe.Coerce (unsafeCoerce)

trackedAs :: forall s o. Newtype s String =>
  (s ~~> o) ->
  Predicate ->
  TypedTripleGetter s o
trackedAs = flip constructTripleGetter

constructTripleGetter :: forall s o. Newtype s String =>
  Predicate ->
  (s ~~> o) ->
  TypedTripleGetter s o
constructTripleGetter pn objectsGetter = TypedTripleGetter pn tripleGetter where
  tripleGetter :: TripleGetter s o
  tripleGetter id = do
    b <- memorizeQueryResults
    if b
      then do
        mt <- liftEffect (lookupInTripleIndex (unsafeCoerce id) pn)
        case mt of
          Nothing -> do
            (object :: Array o) <- lift $ objectsGetter id
            t <- liftEffect (addToTripleIndex
              (unsafeCoerce id)
              pn
              (unsafeCoerce object)
              []
              []
              (\s -> throwError (error ("Basic triple recomputed: " <> s <> " - " <> pn))))
            lift $ lift $ detectCycles $ TripleRef {subject: unwrap id, predicate: pn}
            pure $ ((unsafeCoerce t) :: Triple s o)
          (Just t) -> pure ((unsafeCoerce t) :: Triple s o)
      else do
        (object :: Array o) <- lift $ objectsGetter id
        pure (Triple{ subject: id
                  , predicate: pn
                  , object: object
                  , dependencies: []
                  , supports : []
                  , tripleGetter: tripleGetter
                  })

tripleGetterFromTripleGetter :: forall s o t. Newtype s String => (s **> o) -> String -> (Array o -> s -> Array t) -> (s **> t)
tripleGetterFromTripleGetter (TypedTripleGetter nameOfp p) name f = memorize tripleGetter name where
  tripleGetter :: TripleGetter s t
  tripleGetter id = do
    (Triple{subject, predicate, object}) <- p id
    t <- liftEffect (addToTripleIndex
      (unsafeCoerce id)
      name
      (unsafeCoerce (f object id))
      []
      [TripleRef {subject: unsafeCoerce subject, predicate: predicate}]
      (unsafeCoerce tripleGetter))
    lift $ lift $ detectCycles $ TripleRef {subject: unsafeCoerce id, predicate: name}
    pure $ ((unsafeCoerce t) :: Triple s t)

-- | Construct a TripleGetter from an ObjectsGetter, that is supported by a Triple returned by an arbitrary
-- | TripleGetter. In this way we can insert a computed (rather than calculated by a query) Triple in the
-- | dependency tracking store and have it recomputed when the support changes value.
constructTripleGetterWithArbitrarySupport :: forall s o p. Newtype s String =>
  Predicate ->
  (s -> MonadPerspectivesQuery (Array o)) ->
  TypedTripleGetter s p ->
  TypedTripleGetter s o
constructTripleGetterWithArbitrarySupport pn objectsGetter (TypedTripleGetter predicate supportGetter) = memorize tripleGetter pn where
  tripleGetter :: TripleGetter s o
  tripleGetter id = do
    (object :: Array o) <- objectsGetter id
    t <- liftEffect $ addToTripleIndex
      (unsafeCoerce id)
      pn
      (unsafeCoerce object)
      []
      [TripleRef {subject: (unsafeCoerce id), predicate: predicate}]
      (unsafeCoerce tripleGetter)
    pure $ ((unsafeCoerce t) :: Triple s o)
