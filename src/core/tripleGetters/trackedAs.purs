module Perspectives.TripleGetters.TrackedAs where

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (lift)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Perspectives.CoreTypes (Triple(..), TripleGetter, TripleRef(..), TypedTripleGetter(..), ObjectsGetter)
import Perspectives.EntiteitAndRDFAliases (Predicate)
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter)
import Perspectives.TripleAdministration (addToTripleIndex, detectCycles, lookupInTripleIndex, memorizeQueryResults)
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
