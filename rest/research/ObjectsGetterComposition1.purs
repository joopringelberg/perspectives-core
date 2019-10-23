module Perspectives.ObjectsGetterComposition1 where

import Data.Array (foldM, foldMap, intersect, singleton, union, cons)
import Data.Maybe (maybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF)
import Perspectives.CoreTypes (ObjectsGetter, TripleRef(..))
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.ObjectGetterLookup (lookupObjectsGetterName)
import Perspectives.TripleAdministration1 (withTracking, addToTrackedObjectsIndex)
import Prelude (class Show, pure, show, (<<<), (==), (>=>), (>>=), (<$>), (<>))

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

unionOfObjects' :: forall e.
  ObjectsGetter e ->
  ObjectsGetter e ->
  ObjectsGetter e
unionOfObjects' p q id = withTracking p id >>=
  (\objectsOfP -> let
    combinationResult = foldM
      (\r objectOfP -> withTracking q objectOfP >>= pure <<< union r)
      []
      objectsOfP
    nameOfP = maybe "" id lookupObjectsGetterName p
    nameOfQ = maybe "" id lookupObjectsGetterName q
    ignore = addToTrackedObjectsIndex
      id
      (nameOfP <> "/-/" <> nameOfQ)
      combinationResult
      []
      cons (TripleRef id nameOfP)
        TripleRef nameOfQ <$> objectsOfP
    in combinationResult
  )

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
