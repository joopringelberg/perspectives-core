module Research.RecordAsFunctor where

import Data.Array.Partial (head)
import Prelude (class Apply, class Functor, map, (<*>), ($))

newtype Triples a = Triples
  { subject :: String
  , predicate :: String
  , objects :: Array a
  , supports :: Array TripleRef
  , dependencies :: Array TripleRef}

newtype TripleRef = TripleRef { subject :: String, predicate :: String}

instance functorTriples :: Functor Triples where
  map fn (Triples t@{objects : objs}) = Triples t { objects = map fn objs}

instance applyTriples :: Apply Triples where
  apply (Triples{predicate, objects: fun}) (Triples{subject, objects: val}) =
    head objects
    where
      (Triples{objects}) = head $ fun <*> val

-- Wat levert een PropertyGetter op? Triples. Dus property p toegepast op resource r levert op:
-- <r p <een waarde> <supports> <dependencies> >
-- Maar hier zijn beide in een Array verpakt.
