module Perspectives.PropertyComposition where


import Data.Array (cons, foldr, nub)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Location (Location, functionName, memorize, nameFunction, nestLocationInMonad, saveInLocation, (>==>))
import Perspectives.LocationT (LocationT(..))
import Perspectives.Property (AsyncPropDefsM, NestedLocation, StackedLocation, StackedMemorizingPluralGetter, StackedMemorizingSingleGetter)
import Perspectives.Resource (locationFromMaybeResource)
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

-- | Use this function to lift a SingleGetter or PluralGetter to a StackedMemorizingSingleGetter or a
-- | StackedMemorizingPluralGetter.
memorizeInStackedLocation :: forall b e.
  (Maybe Resource -> (AsyncPropDefsM e) b)
  -> (Maybe Resource -> StackedLocation e b)
memorizeInStackedLocation f = nameFunction (functionName f)(\mr -> LocationT do
    loc <- locationFromMaybeResource mr
    g loc)
  where
  g = nestLocationInMonad f

memorizeSingleResourceGetter :: forall e.
  (Maybe Resource -> (AsyncPropDefsM e) (Location (Maybe Resource)))
  -> (Maybe Resource -> StackedLocation e (Maybe Resource))
memorizeSingleResourceGetter f = nameFunction (functionName f)(\mr -> LocationT do
    loc <- locationFromMaybeResource mr
    g loc)
  where
  g = memorize f

-- magic f mr = nestedToStackedLocation $ bind (locationFromMaybeResource mr) (nestLocationInMonad f)

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
    applyg = nameFunction (functionName g )(traverse (g <<< Just))

    collectResults :: forall e. Array (Maybe a) -> StackedLocation e (Array a)
    collectResults = pure <<< nub <<< (foldr (maybe id cons) [])

  -- Important: the association is to the right, i.e. the expression below equals:
  -- (f >=> (applyg >==> collectResults))
  -- The name preserving composeKleisli application means the subexpression
  -- (applyg >==> collectResults)
  -- bears the name of applyg, casu quo the name of g. As f has a name, too, here we will
  -- see three locations connected by names relating to f and g, respectively.
  in nameFunction name (f >=> applyg >==> collectResults)

  -- in nameFunction name (f >=> applyg >=> collectResults)
  -- in (\r -> f r >>= nameFunction ("applyg" <> functionName g)
  --     (\x -> applyg x >>= nameFunction ("collectResults" <> functionName g)
  --       (\y -> collectResults y)))

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
    name = functionName f <> ">>->>" <> functionName g

    applyg :: forall e. Array Resource -> StackedLocation e (Array (Array a))
    applyg = nameFunction (functionName g) (traverse (g <<< Just))

    collectResults :: forall e. Array (Array a) -> StackedLocation e (Array a)
    collectResults = pure <<< nub <<< join

  in nameFunction name (f >=> applyg >==> collectResults)

infixl 0 pTop as >>->>
