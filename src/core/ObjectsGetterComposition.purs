module Perspectives.ObjectsGetterComposition where

import Data.Array (foldM, foldMap, intersect, singleton, union)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF)
import Perspectives.CoreTypes (type (~~>), ObjectsGetter, MP)
import Perspectives.PerspectivesTypesInPurescript (PBool(..))
import Prelude (class Eq, pure, show, (<<<), (==), (>=>), (>>=), (>>>))

unionOfObjects :: forall s o t e.
  Eq t =>
  (s ~~> o) e ->
  (o ~~> t) e ->
  (s ~~> t) e
unionOfObjects p q = p >=>
  (\(objectsOfP :: Array o) -> foldM
      (\(r :: Array t) (objectOfP :: o) -> q objectOfP >>= pure <<< union r)
      []
      objectsOfP)

infixl 9 unionOfObjects as /-/

unionOfObjects' :: forall s o t e.
  Eq t =>
  (s ~~> o) e ->
  (o ~~> t) e ->
  (s ~~> t) e
unionOfObjects' p q = p >=> applyToAll q

applyToAll :: forall s o t e. Eq t =>
  (o ~~> t) e ->
  (Array o -> MP e (Array t))
applyToAll q objectsOfP = foldM
  (\(r :: Array t) (objectOfP :: o) -> q objectOfP >>= pure <<< union r)
  []
  objectsOfP

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
composeMonoidal :: forall s o e a.
  (s ~~> o) e
  -> (Array o -> a)
  -> (s ~~> a) e
composeMonoidal p f = p >=> pure <<< singleton <<< f

contains :: forall s o e. Eq o => o -> (s ~~> o) e -> (s ~~> PBool) e
contains obj p = p `composeMonoidal` (alaF Disj foldMap ((==) obj) >>> show >>> PBool)
