module Perspectives.Triple where

import Prelude

import Perspectives.CoreTypes (MonadPerspectivesQuery, TripleRef, TripleGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (Subject, Predicate)

newtype MPObjects e a = MPObjects (MonadPerspectivesQuery (AjaxAvarCache e) (Array a))

instance functorMPObjects :: Functor (MPObjects e) where
  map f (MPObjects mpo) = MPObjects do
    arr <- mpo
    pure $ map f arr

instance applyMPObjects :: Apply (MPObjects e) where
  apply (MPObjects mpf) (MPObjects mpo) = MPObjects do
    f <- mpf
    o <- mpo
    pure $ f <*> o

newtype Triple' e a = Triple'
  { subject :: Subject
  , predicate :: Predicate
  , object :: Array a
  , dependencies :: Array TripleRef
  , supports :: Array TripleRef
  , tripleGetter :: TripleGetter e}

type Triple e = Triple' e String

instance functorTriple :: Functor (Triple' e) where
  map f (Triple' tf@{object}) = Triple' $ tf {object = map f object}

instance applyTriple :: Apply (Triple' e) where
  apply (Triple'{object: f}) (Triple' tf@{object}) = Triple' $ tf {object = apply f object}

newtype MPTriple e a = MPTriple (MonadPerspectivesQuery (AjaxAvarCache e) (Triple' e a))

instance functorMPTriple :: Functor (MPTriple e) where
  map f (MPTriple mpt) = MPTriple do
    t <- mpt
    pure $ map f t

instance applyMPTriple :: Apply (MPTriple e) where
  apply (MPTriple mpf) (MPTriple mpo) = MPTriple do
    f <- mpf
    o <- mpo
    pure $ apply f o
