module Perspectives.ObjectsGetterComposition where

import Data.Array (foldM, foldMap, intersect, singleton, union)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF)
import Perspectives.CoreTypes (ObjectsGetter)
import Prelude (class Eq, class Show, pure, (<<<), (==), (>=>), (>>=))

unionOfObjects :: forall s o t e.
  Eq o =>
  ObjectsGetter s t e ->
  ObjectsGetter t o e ->
  ObjectsGetter s o e
unionOfObjects p q = p >=>
  (\objectsOfP -> foldM
      (\r objectOfP -> q objectOfP >>= pure <<< union r)
      []
      objectsOfP)

infixl 9 unionOfObjects as /-/

intersectionOfObjects :: forall s o t e.
  Eq o =>
  ObjectsGetter s t e ->
  ObjectsGetter t o e ->
  ObjectsGetter s o e
intersectionOfObjects p q = p >=>
  (\objectsOfP -> foldM
      (\r objectOfP -> q objectOfP >>= pure <<< intersect r)
      []
      objectsOfP)

infixl 9 intersectionOfObjects as \-\

-- | Compose an ObjectsGetter from an ObjectsGetter and a function
-- | that maps an Array String value to a value that can be shown.
-- | This function typically folds over a monoid.
composeMonoidal :: forall s o e a. Show a =>
  ObjectsGetter s o e
  -> (Array o -> a)
  -> ObjectsGetter s a e
composeMonoidal p f = p >=> pure <<< singleton <<< f

contains :: forall s o e.
  Eq o =>
  o ->
  ObjectsGetter s o e ->
  ObjectsGetter s Boolean e
contains obj p = p `composeMonoidal` (alaF Disj foldMap ((==) obj))
