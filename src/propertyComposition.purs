module Perspectives.PropertyComposition where


import Data.Array (cons, foldr, nub)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Location (Location, connectLocations, functionName, saveInLocation, locationDependent, locationValue, nameFunction, nestLocationInMonad)
import Perspectives.LocationT (LocationT(..))
import Perspectives.Property (AsyncPropDefsM, MemorizingPluralGetter, MemorizingSingleGetter, NestedLocation, StackedLocation)
import Perspectives.Resource (locationFromResource)
import Perspectives.ResourceTypes (Resource)
import Prelude (bind, id, join, pure, ($), (<$>), (<<<), (<>), (>=>), (>>>))

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
sTos :: forall a. MemorizingSingleGetter Resource -> MemorizingSingleGetter a -> MemorizingSingleGetter a
sTos p q = nameFunction name (p >=> q) where
  name :: String
  name = functionName p <> ">->" <> functionName q

infixl 0 sTos as >->

-- sTop :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does not memorize. The memorizing is done entirely by its arguments.
sTop :: forall a. MemorizingSingleGetter Resource -> MemorizingPluralGetter a -> MemorizingPluralGetter a
sTop p q = nameFunction name (p >=> q) where
  name :: String
  name = functionName p <> ">->" <> functionName q

infixl 0 sTop as >->>

-- pTos :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does memorize the result of applying the second argument to the result of the first.
pTos :: forall a. Eq a => MemorizingPluralGetter Resource -> MemorizingSingleGetter a -> MemorizingPluralGetter a
pTos f g =
  let
    h :: forall e. Location (Array Resource) -> AsyncPropDefsM e (Location (Array a))
    h arrayB = case locationDependent h arrayB of
      Nothing -> do
        (p :: Array (Location (Maybe a))) <- traverse (locationFromResource >=> g) (locationValue arrayB)
        -- Here we connect the result F of applying f with the result of applying g to F.
        pure $ connectLocations arrayB (functionName g) (saveInLocation $ nub $ foldr (locationValue >>> (maybe id cons)) [] p)
      (Just loc) -> pure loc
    name :: String
    name = functionName f <> ">>->" <> functionName g

    -- Note that the function h is named only once, when the composition operator is applied.
    in nameFunction name (f >=> h)

infixl 0 pTos as >>->

-- pTop :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does memorize the result of applying the second argument to the result of the first.
-- TODO: Zodra dit in StackedMemorizingPluralGetter is geschreven, is connectLocations niet nodig.
-- composekleisli verbindt f x met (f >=>g) x.
pTop :: forall a. Eq a => MemorizingPluralGetter Resource -> MemorizingPluralGetter a -> MemorizingPluralGetter a
pTop f g =
  let
    h :: forall e. Location (Array Resource) -> AsyncPropDefsM e (Location (Array a))
    h arrayB = case locationDependent h arrayB of
      Nothing -> do
                  (p :: Array (Location (Array a))) <- traverse (locationFromResource >=> g) (locationValue arrayB)
                  pure $ connectLocations arrayB (functionName g) (saveInLocation $ nub (join (locationValue <$> p)))
      (Just loc) -> pure loc
    name :: String
    name = functionName f <> ">>->" <> functionName g

    -- Note that the function h is named only once, when the composition operator is applied.
    in nameFunction name (f >=> h)

infixl 0 pTop as >>->>
