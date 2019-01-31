module Perspectives.ObjectsGetterComposition where

import Data.Array (foldM, foldMap, intersect, singleton, union)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF)
import Perspectives.CoreTypes (ObjectsGetter)
import Prelude (class Eq, class Show, pure, (<<<), (==), (>=>), (>>=))

unionOfObjects :: forall s o t c r b e.
  Eq o =>
  ObjectsGetter s t c r b e ->
  ObjectsGetter t o c r b e ->
  ObjectsGetter s o c r b e
unionOfObjects p q = p >=>
  (\objectsOfP -> foldM
      (\r objectOfP -> q objectOfP >>= pure <<< union r)
      []
      objectsOfP)

infixl 9 unionOfObjects as /-/

intersectionOfObjects :: forall s o t c r b e.
  Eq o =>
  ObjectsGetter s t c r b e ->
  ObjectsGetter t o c r b e ->
  ObjectsGetter s o c r b e
intersectionOfObjects p q = p >=>
  (\objectsOfP -> foldM
      (\r objectOfP -> q objectOfP >>= pure <<< intersect r)
      []
      objectsOfP)

infixl 9 intersectionOfObjects as \-\

-- | Compose an ObjectsGetter from an ObjectsGetter and a function
-- | that maps an Array String value to a value that can be shown.
-- | This function typically folds over a monoid.
composeMonoidal :: forall s o c r b e a. Show a =>
  ObjectsGetter s o c r b e
  -> (Array o -> a)
  -> ObjectsGetter s a c r b e
composeMonoidal p f = p >=> pure <<< singleton <<< f

contains :: forall s o c r b e.
  Eq o =>
  o ->
  ObjectsGetter s o c r b e ->
  ObjectsGetter s Boolean c r b e
contains obj p = p `composeMonoidal` (alaF Disj foldMap ((==) obj))
