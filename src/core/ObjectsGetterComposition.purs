module Perspectives.ObjectsGetterComposition where

import Data.Array (foldM, foldMap, intersect, singleton, union)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF)
import Perspectives.CoreTypes (ObjectsGetter, MP, type (~~>))
import Perspectives.PerspectivesTypes (PBool(..))
import Prelude (class Eq, pure, show, (<<<), (==), (>=>), (>>=), (>>>))

unionOfObjects :: forall s o t e.
  Eq t =>
  (s ~~> o) e ->
  (o ~~> t) e ->
  (s ~~> t) e
unionOfObjects p q = p >=> applyToAll q

applyToAll :: forall o t e. Eq t =>
  (o ~~> t) e ->
  (Array o -> MP e (Array t))
applyToAll q objectsOfP = foldM
  (\(r :: Array t) (objectOfP :: o) -> q objectOfP >>= pure <<< union r)
  []
  objectsOfP

infixl 9 unionOfObjects as /-/

intersectionOfObjects :: forall s o t e.
  Eq t =>
  (s ~~> o) e ->
  (o ~~> t) e ->
  (s ~~> t) e
intersectionOfObjects p q = p >=>
  (\objectsOfP -> foldM
      (\r objectOfP -> q objectOfP >>= pure <<< intersect r)
      []
      objectsOfP)

infixl 9 intersectionOfObjects as \-\

-- | Compose an ObjectsGetter from an ObjectsGetter and a function
-- | that maps an Array o value to an aggregated value.
-- | This function typically folds over a monoid.
composeMonoidal :: forall s o e a.
  (s ~~> o) e
  -> (Array o -> a)
  -> (s ~~> a) e
composeMonoidal p f = p >=> pure <<< singleton <<< f

contains :: forall s o e. Eq o => o -> (s ~~> o) e -> (s ~~> PBool) e
contains obj p = p `composeMonoidal` (alaF Disj foldMap ((==) obj) >>> show >>> PBool)
