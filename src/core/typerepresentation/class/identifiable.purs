module Perspectives.Representation.Class.Identifiable where

import Data.Newtype (class Newtype, unwrap)

class Newtype i String <= Identifiable e i where
  identifier :: e -> i

identifier_ :: forall e i. Identifiable e i => e -> String
identifier_ x = unwrap (identifier x :: i)
