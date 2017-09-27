module Perspectives.PropertyComposition where


import Data.Array (cons, foldr, nub)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Location (Location, functionName, saveInLocation, nameFunction)
import Perspectives.LocationT (LocationT(..))
import Perspectives.Property (AsyncPropDefsM, NestedLocation, StackedLocation, StackedMemorizingSingleGetter, StackedMemorizingPluralGetter)
import Perspectives.ResourceTypes (Resource)
import Prelude (bind, id, join, pure, ($), (<<<), (<>), (>=>))

affToStackedLocation :: forall e a. AsyncPropDefsM e a -> StackedLocation e a
affToStackedLocation ma = LocationT (bind ma (\a -> pure $ saveInLocation a))

locationToStackedLocation :: forall e a. Location a -> StackedLocation e a
locationToStackedLocation la = LocationT (pure la)

locationToNestedLocation :: forall a e. Location a -> NestedLocation e a
locationToNestedLocation la = pure la

nestedToStackedLocation :: forall e a. NestedLocation e a -> StackedLocation e a
nestedToStackedLocation ma = LocationT ma

stackedToNestedLocation :: forall e a. StackedLocation e a -> NestedLocation e a
stackedToNestedLocation (LocationT ma) = ma

-- infixl 0 nestLocationInMonad as |->

-- infixl 0 nestLocationInMonad as |->>

lowerFromLocationT :: forall a x e.
  (x -> LocationT (AsyncPropDefsM e) a)
  ->
  (x -> AsyncPropDefsM e (Location a))
lowerFromLocationT f = stackedToNestedLocation <<< f

liftToLocationT :: forall a e.
  (Location (Maybe Resource) -> AsyncPropDefsM e (Location a))
  ->
  (Location (Maybe Resource) -> LocationT (AsyncPropDefsM e) a)
liftToLocationT f = nestedToStackedLocation <<< f

-- sTos :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Maybe c)))
-- | This composition operator does not memorize. The memorizing is done entirely by its arguments.
sTos :: forall a. StackedMemorizingSingleGetter Resource -> StackedMemorizingSingleGetter a -> StackedMemorizingSingleGetter a
sTos p q = nameFunction name (p >=> q) where
  name :: String
  name = functionName p <> ">->" <> functionName q

infixl 0 sTos as >->

-- sTop :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does not memorize. The memorizing is done entirely by its arguments.
sTop :: forall a. StackedMemorizingSingleGetter Resource -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
sTop p q = nameFunction name (p >=> q) where
  name :: String
  name = functionName p <> ">->" <> functionName q

infixl 0 sTop as >->>

-- pTos :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does memorize the result of applying the second argument to the result of the first.
pTos :: forall a. Eq a => StackedMemorizingPluralGetter Resource -> StackedMemorizingSingleGetter a -> StackedMemorizingPluralGetter a
pTos f g =
  let
    name :: String
    name = functionName f <> ">>->" <> functionName g

    applyg :: forall e. Array Resource -> StackedLocation e (Array (Maybe a))
    applyg = nameFunction "applyg" traverse (g <<< Just)

    collectResults :: forall e. Array (Maybe a) -> StackedLocation e (Array a)
    collectResults = nameFunction "collectResults" pure <<< nub <<< (foldr (maybe id cons) [])

  in nameFunction name (f >=> applyg >=> collectResults)

infixl 0 pTos as >>->

-- pTop :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does memorize the result of applying the second argument to the result of the first.
pTop :: forall a. Eq a => StackedMemorizingPluralGetter Resource -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
pTop f g =
  let
    name :: String
    name = functionName f <> ">>->" <> functionName g

    applyg :: forall e. Array Resource -> StackedLocation e (Array (Array a))
    applyg = nameFunction "applyg" traverse (g <<< Just)

    collectResults :: forall e. Array (Array a) -> StackedLocation e (Array a)
    collectResults = nameFunction "collectResults" pure <<< nub <<< join

  in nameFunction name (f >=> applyg >=> collectResults)

infixl 0 pTop as >>->>
