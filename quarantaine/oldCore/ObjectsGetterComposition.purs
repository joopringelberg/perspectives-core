module Perspectives.ObjectsGetterComposition where

import Data.Array (foldM, foldMap, intersect, singleton, union)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF)
import Perspectives.CoreTypes (MP, type (~~>))
import Prelude (class Eq, pure, show, (<<<), (==), (>=>), (>>=), (>>>))

unionOfObjects :: forall s o t.
  Eq t =>
  (s ~~> o) ->
  (o ~~> t) ->
  (s ~~> t)
unionOfObjects p q = p >=> applyToAll q

applyToAll :: forall o t. Eq t =>
  (o ~~> t) ->
  (Array o -> MP (Array t))
applyToAll q objectsOfP = foldM
  (\(r :: Array t) (objectOfP :: o) -> q objectOfP >>= pure <<< union r)
  []
  objectsOfP

infixl 9 unionOfObjects as /-/

intersectionOfObjects :: forall s o t.
  Eq t =>
  (s ~~> o) ->
  (o ~~> t) ->
  (s ~~> t)
intersectionOfObjects p q = p >=>
  (\objectsOfP -> foldM
      (\r objectOfP -> q objectOfP >>= pure <<< intersect r)
      []
      objectsOfP)

infixl 9 intersectionOfObjects as \-\

-- | Compose an ObjectsGetter from an ObjectsGetter and a function
-- | that maps an Array o value to an aggregated value.
-- | This function typically folds over a monoid.
composeMonoidal :: forall s o a.
  (s ~~> o)
  -> (Array o -> a)
  -> (s ~~> a)
composeMonoidal p f = p >=> pure <<< singleton <<< f

-- Example: contains. A more efficient implementation is available in Perspectives.ObjectGetterConstructrors.
-- contains :: forall s o. Eq o => o -> (s ~~> o) -> (s ~~> PBool)
-- contains obj p = p `composeMonoidal` (alaF Disj foldMap ((==) obj) >>> show >>> PBool)
