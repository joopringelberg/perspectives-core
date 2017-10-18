module Perspectives.Triples where

import Control.Monad.Eff.Class (liftEff)
import Data.Argonaut (Json, toArray, toBoolean, toNumber, toString)
import Data.Maybe (Maybe(..))
import Perspectives.Property (AsyncPropDefsM, PropertyName, getPluralGetter, getSingleGetter)
import Perspectives.ResourceTypes (Resource(..), ResourceId)
import Perspectives.TripleAdministration (class TripleStore, Triple(..), ResourceIndex, lookup)
import Prelude (bind, otherwise, pure)

data NamedFunction f = NamedFunction String f

runQuery :: forall a b. NamedFunction (a -> b) -> a -> b
runQuery (NamedFunction _ f) res = f res

type TripleGetter e a = Maybe Resource -> AsyncPropDefsM e (Triple (Maybe a))

constructTripleGetter :: forall a e r. TripleStore r => (Json -> Maybe a) -> PropertyName -> r -> NamedFunction (TripleGetter e a)
constructTripleGetter tofn pn tripleStore = NamedFunction pn tripleGetter where
  -- Here we interpret the empty string as the identification of Nothing??
  tripleGetter ::  TripleGetter e a
  tripleGetter res@(Just (Resource{id})) = do
    t@(Triple{object}) <- liftEff (lookup tripleStore id pn)
    case object of
      (Just _) -> pure t
      otherwise -> do
        (ma :: Maybe a) <- getSingleGetter tofn pn res
        pure (Triple{ subject: id
                , predicate: pn
                , object: ma
                , supports: []
                , dependencies: []
                })
  tripleGetter Nothing = pure (Triple{ subject: ""
          , predicate: pn
          , object: Nothing
          , supports: []
          , dependencies: []
          })

type TriplesGetter e a = Maybe Resource -> AsyncPropDefsM e (Triple (Array a))

constructTriplesGetter :: forall a e. (Json -> Maybe a) -> PropertyName -> NamedFunction (TriplesGetter e a)
constructTriplesGetter tofn pn = NamedFunction pn triplesGetter where
  -- Here we interpret the empty string as the identification of Nothing??
  triplesGetter ::  TriplesGetter e a
  triplesGetter res@(Just (Resource{id})) = do
    (ma :: Array a) <- getPluralGetter tofn pn res
    pure (Triple{ subject: id
            , predicate: pn
            , object: ma
            , supports: []
            , dependencies: []
            })
  triplesGetter Nothing = pure (Triple{ subject: ""
          , predicate: pn
          , object: []
          , supports: []
          , dependencies: []
          })
