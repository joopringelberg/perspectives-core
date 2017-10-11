module Perspectives.PropertyComposition where


import Control.Monad.Aff (Canceler(..), launchAff, runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (ST)
import Data.Array (cons, elemIndex, head, tail, union)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Network.HTTP.Affjax (AJAX)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Location (Location, THEORYDELTA, functionName, locationValue, memorize, nameFunction, nestLocationInMonad, saveInLocation, setLocationValue, (>==>), addDependent, setUpdateFunction)
import Perspectives.LocationT (LocationT(..), copyToLocation)
import Perspectives.Property (AsyncPropDefsM, NestedLocation, StackedLocation, StackedMemorizingPluralGetter, StackedMemorizingSingleGetter)
import Perspectives.Resource (PROPDEFS, ResourceIndex, locationFromMaybeResource)
import Perspectives.ResourceTypes (Resource)
import Prelude (Unit, bind, const, eq, id, pure, unit, ($), (<$>), (<*>), (<<<), (<>), (==), (>>=))

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

-- | From a function that takes a Resource and returns a Resource, create a function that
-- | connects the locations that these Resources are saved in. The resulting function bears
-- | the same name as its argument function.
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
sTos :: forall a. Eq a => StackedMemorizingSingleGetter Resource -> StackedMemorizingSingleGetter a -> StackedMemorizingSingleGetter a
-- sTos p q = nameFunction name (p >==> q) where
--   name :: String
--   name = functionName p <> ">->" <> functionName q
sTos p q a = nestedToStackedLocation $ do
  (b :: Location (Maybe Resource)) <- stackedToNestedLocation $ p a
  (c :: Location (Maybe a)) <- stackedToNestedLocation $ q (locationValue b)
  -- Now copy c to a fresh location, making it depend on c.
  (r :: Location (Maybe a)) <- pure ((nameFunction "id" id) <$> c)
  -- Now make it a dependent of b, too.
  _ <- liftEff $ addDependent b "some name" r
  -- Finally, install an appropriate update function in r.
  _ <- liftEff $ setUpdateFunction r (update b c r)
  pure r
  where
    -- update :: forall e. Location (Maybe Resource) -> Location (Maybe a) -> Location (Maybe a) -> Eff (Magic e) (Canceler (Magic e))
    update b c r = (runAff (\e-> pure unit) (\z-> pure unit)) $ do
      case locationValue c == locationValue r of
        true -> do
          c' <- stackedToNestedLocation $ q (locationValue b)
          _ <- liftEff $ setLocationValue r (locationValue c')
          pure unit
        false -> liftEff $ setLocationValue r (locationValue c)

type Magic e =  (gm :: GLOBALMAP, avar :: AVAR, ajax :: AJAX, prd :: PROPDEFS, st :: (ST ResourceIndex), td :: THEORYDELTA | e)

copyResult :: forall b e.
  Maybe Resource
  -> StackedLocation e b
  -> StackedLocation e b
copyResult mr computationOfb =
  copyToLocation computationOfb (nestedToStackedLocation $ locationFromMaybeResource mr)

infixl 0 sTos as >->

-- sTop :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does not memorize. The memorizing is done entirely by its arguments.
sTop :: forall a. StackedMemorizingSingleGetter Resource -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
sTop p q = nameFunction name (p >==> q) where
  name :: String
  name = functionName p <> ">->" <> functionName q

infixl 0 sTop as >->>

-- pTos :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does memorize the result of applying the second argument to the result of the first.
pTos :: forall a. Eq a => StackedMemorizingPluralGetter Resource -> StackedMemorizingSingleGetter a -> StackedMemorizingPluralGetter a
pTos f g r = f r >>= nameFunction (functionName g) (\arr -> pTos' (Just arr)) where
  pTos' :: forall e. Maybe (Array Resource) -> StackedLocation (td :: THEORYDELTA | e) (Array a)
  pTos' Nothing = pure []
  pTos' (Just fs) = (nameFunction "mconsUniques" mconsUniques) <$> (g $ head fs) <*> (pTos' $ tail fs)

infixl 0 pTos as >>->

-- NOTE: if we make mcons point free, it will effectively not have a name when applied.
mcons :: forall a. (Maybe a) -> (Array a) -> (Array a)
mcons e a = (maybe id cons) e a

-- NOTE: because of the class constraint, mconsUniques compiles to a function that returns a function.
-- This latter function will NOT have a name. Hence it is necessary to name the function on each application...
mconsUniques :: forall a. Eq a => Maybe a -> Array a -> Array a
mconsUniques (Just el) arr | (maybe true (const false)) $ elemIndex el arr = cons el arr
mconsUniques otherwise arr = arr

-- pTop :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- | This composition operator does memorize the result of applying the second argument to the result of the first.
pTop :: forall a. Eq a => StackedMemorizingPluralGetter Resource -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
pTop f g r = f r >>= nameFunction (functionName g) (\arr -> pTop' (Just arr)) where
  pTop' :: forall e. Maybe (Array Resource) -> StackedLocation (td :: THEORYDELTA | e) (Array a)
  pTop' Nothing = pure []
  pTop' (Just (fs :: Array Resource)) = union <$> (g $ head fs) <*> (pTop' $ tail fs)

infixl 0 pTop as >>->>
