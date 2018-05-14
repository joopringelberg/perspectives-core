module Perspectives.ObjectsGetterComposition where

import Data.Array (foldM, foldMap, intersect, singleton, union)
import Data.Monoid (class Monoid)
import Data.Monoid.Disj (Disj(..))
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
-- | that maps a String to a Monoid value that can be shown.
-- | The (String) results of the ObjectsGetter will be mapped into
-- | the Monoid, then folded, then shown and returned in an Array
-- | wrapped in the monad.
composeMonoidal :: forall e m. Monoid m => Show m =>
  ObjectsGetter e
  -> (String -> m)
  -> ObjectsGetter e
composeMonoidal p f = p >=> pure <<< singleton <<< show <<< foldMap f

contains :: forall e. ID -> ObjectsGetter e -> ObjectsGetter e
contains obj p = p `composeMonoidal` (Disj <<< (==) obj)
