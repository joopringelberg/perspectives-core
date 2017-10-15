module Perspectives.Triples where

import Data.Argonaut (Json, toArray, toBoolean, toNumber, toString)
import Data.Maybe (Maybe(..))
import Perspectives.Property (AsyncPropDefsM, PropertyName, getPluralGetter, getSingleGetter)
import Perspectives.ResourceTypes (Resource(..), ResourceId)
import Prelude (bind, pure)

newtype TripleRef = TripleRef { subject :: ResourceId, predicate :: String}

data NamedFunction f = NamedFunction String f

runQuery :: forall a b. NamedFunction (a -> b) -> a -> b
runQuery (NamedFunction _ f) res = f res

newtype Triple a = Triple
  { subject :: ResourceId
  , predicate :: String
  , object :: Maybe a
  , supports :: Array TripleRef
  , dependencies :: Array TripleRef}

type TripleGetter e a = Maybe Resource -> AsyncPropDefsM e (Triple a)

constructTripleGetter :: forall a e. (Json -> Maybe a) -> PropertyName -> NamedFunction (TripleGetter e a)
constructTripleGetter tofn pn = NamedFunction pn tripleGetter where
  -- Here we interpret the empty string as the identification of Nothing??
  tripleGetter ::  TripleGetter e a
  tripleGetter res@(Just (Resource{id})) = do
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

newtype Triples a = Triples
  { subject :: ResourceId
  , predicate :: String
  , objects :: Array a
  , supports :: Array TripleRef
  , dependencies :: Array TripleRef}

type TriplesGetter e a = Maybe Resource -> AsyncPropDefsM e (Triples a)

constructTriplesGetter :: forall a e. (Json -> Maybe a) -> PropertyName -> NamedFunction (TriplesGetter e a)
constructTriplesGetter tofn pn = NamedFunction pn triplesGetter where
  -- Here we interpret the empty string as the identification of Nothing??
  triplesGetter ::  TriplesGetter e a
  triplesGetter res@(Just (Resource{id})) = do
    (ma :: Array a) <- getPluralGetter tofn pn res
    pure (Triples{ subject: id
            , predicate: pn
            , objects: ma
            , supports: []
            , dependencies: []
            })
  triplesGetter Nothing = pure (Triples{ subject: ""
          , predicate: pn
          , objects: []
          , supports: []
          , dependencies: []
          })
