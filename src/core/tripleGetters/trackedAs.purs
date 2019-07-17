module Perspectives.TripleGetters.TrackedAs where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Perspectives.CoreTypes (ObjectsGetter, StringTriple, StringTripleGetter, StringTypedTripleGetter, Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), MonadPerspectivesQuery)
import Perspectives.EntiteitAndRDFAliases (Predicate)
import Perspectives.TripleAdministration (addToTripleIndex, detectCycles, lookupInTripleIndex, memorize, memorizeQueryResults)
import Prelude (bind, flip, pure, show, ($), (<>), discard)

trackedAs ::
  ObjectsGetter ->
  Predicate ->
  StringTypedTripleGetter
trackedAs = flip constructTripleGetter

constructTripleGetter ::
  Predicate ->
  ObjectsGetter ->
  StringTypedTripleGetter
constructTripleGetter pn objectsGetter = TypedTripleGetter pn tripleGetter where
  tripleGetter :: TripleGetter String String
  tripleGetter id = do
    b <- memorizeQueryResults
    if b
      then do
        mt <- liftEffect (lookupInTripleIndex id pn)
        case mt of
          Nothing -> do
            (object :: Array String) <- lift $ objectsGetter id
            t <- liftEffect (addToTripleIndex
              id
              pn
              ( object)
              []
              []
              (\s -> throwError (error ("Basic triple recomputed: " <> s <> " - " <> pn))))
            lift $ lift $ detectCycles $ TripleRef {subject: show id, predicate: pn}
            pure $ (( t) :: Triple String String)
          (Just t) -> pure (t :: Triple String String)
      else do
        (object :: Array String) <- lift $ objectsGetter id
        pure (Triple{ subject: id
                  , predicate: pn
                  , object: object
                  , dependencies: []
                  , supports : []
                  , tripleGetter: tripleGetter
                  })

tripleGetterFromTripleGetter :: StringTypedTripleGetter -> String -> (Array String -> String -> Array String) -> StringTypedTripleGetter
tripleGetterFromTripleGetter (TypedTripleGetter nameOfp p) name f = memorize tripleGetter name where
  tripleGetter :: StringTripleGetter
  tripleGetter id = do
    (Triple{subject, predicate, object}) <- p id
    t <- liftEffect (addToTripleIndex
      id
      name
      (f object id)
      []
      [TripleRef {subject: subject, predicate: predicate}]
      tripleGetter)
    lift $ lift $ detectCycles $ TripleRef {subject: id, predicate: name}
    pure $ (t :: StringTriple)

-- | Construct a TripleGetter from an ObjectsGetter, that is supported by a Triple returned by an arbitrary
-- | TripleGetter. In this way we can insert a computed (rather than calculated by a query) Triple in the
-- | dependency tracking store and have it recomputed when the support changes value.
constructTripleGetterWithArbitrarySupport ::
  Predicate ->
  (String -> MonadPerspectivesQuery (Array String)) ->
  StringTypedTripleGetter ->
  StringTypedTripleGetter
constructTripleGetterWithArbitrarySupport pn objectsGetter (TypedTripleGetter predicate supportGetter) = memorize tripleGetter pn where
  tripleGetter :: StringTripleGetter
  tripleGetter id = do
    (object :: Array String) <- objectsGetter id
    t <- liftEffect $ addToTripleIndex
      id
      pn
      object
      []
      [TripleRef {subject: id, predicate: predicate}]
      tripleGetter
    pure $ (t :: StringTriple)
