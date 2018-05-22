module Perspectives.ObjectsGetterComposition where

import Data.Array (foldM, foldMap, intersect, singleton, union)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF)
import Perspectives.CoreTypes (ObjectsGetter)
import Perspectives.EntiteitAndRDFAliases (ID)
import Prelude (class Show, pure, show, (<<<), (==), (>=>), (>>=))

unionOfObjects :: forall e.
  ObjectsGetter e ->
  ObjectsGetter e ->
  ObjectsGetter e
unionOfObjects p q = p >=>
  (\objectsOfP -> foldM
      (\r objectOfP -> q objectOfP >>= pure <<< union r)
      []
      objectsOfP)

infixl 9 unionOfObjects as /-/

intersectionOfObjects :: forall e.
  ObjectsGetter e ->
  ObjectsGetter e ->
  ObjectsGetter e
intersectionOfObjects p q = p >=>
  (\objectsOfP -> foldM
      (\r objectOfP -> q objectOfP >>= pure <<< intersect r)
      []
      objectsOfP)

infixl 9 intersectionOfObjects as \-\

-- | Compose an ObjectsGetter from an ObjectsGetter and a function
-- | that maps an Array String value to a value that can be shown.
-- | This function typically folds over a monoid.
composeMonoidal :: forall e a. Show a =>
  ObjectsGetter e
  -> (Array String -> a)
  -> ObjectsGetter e
composeMonoidal p f = p >=> pure <<< singleton <<< show <<< f

contains :: forall e. ID -> ObjectsGetter e -> ObjectsGetter e
contains obj p = p `composeMonoidal` (alaF Disj foldMap ((==) obj))
