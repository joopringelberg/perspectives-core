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
import Perspectives.ObjectCollection (class ObjectCollection)
import Perspectives.Property (AsyncPropDefsM, NestedLocation, StackedLocation, StackedMemorizingPluralGetter, StackedMemorizingSingleGetter)
import Perspectives.Resource (PROPDEFS, ResourceIndex, locationFromMaybeResource)
import Perspectives.ResourceTypes (Resource(..))
import Perspectives.TripleAdministration (NamedFunction(..), Triple(..), TripleGetter, TripleRef(..), addDependency, addTriple1)
import Perspectives.Triples (NamedSingleTripleGetter)
import Prelude (Unit, bind, const, eq, id, pure, unit, ($), (<$>), (<*>), (<<<), (<>), (==), (>=>), (>>=), discard)

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

class (ObjectCollection ef1, ObjectCollection ef2, ObjectCollection ef3) <= ObjectCollectionCombination ef1 ef2 ef3 where
  compose :: forall e a. ObjectCollection ef3 => Eq a =>
    NamedFunction (TripleGetter e (ef1 Resource)) ->
    NamedFunction (TripleGetter e (ef2 a)) ->
    NamedFunction (TripleGetter e (ef3 a))

instance singleToSingle :: ObjectCollectionCombination Maybe Maybe Maybe where
  compose = sTos

-- instance singleToPlural :: TypedObjectCollection Maybe Array Array where
--   compose = sTop

-- sTos :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Maybe c)))
-- | This composition operator does not memorize. The memorizing is done entirely by its arguments.
sTos' :: forall a. Eq a => NamedSingleTripleGetter Resource -> NamedSingleTripleGetter a -> NamedSingleTripleGetter a
sTos' (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name (p >=> (\(Triple{object}) -> q object)) where
  name :: String
  name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

sTos :: forall e a. Eq a =>
  NamedFunction (TripleGetter e (Maybe Resource)) ->
  NamedFunction (TripleGetter e (Maybe a)) ->
  NamedFunction (TripleGetter e (Maybe a))
-- sTos :: forall a. Eq a => NamedSingleTripleGetter Resource -> NamedSingleTripleGetter a -> NamedSingleTripleGetter a
sTos (NamedFunction nameOfp p) (NamedFunction nameOfq q) = NamedFunction name combination where

  combination :: TripleGetter e (Maybe a)
  combination mr@(Just (Resource{id})) = do
    first@(Triple{object: mo, supports: psupports, dependencies: pdependencies}) <- p mr
    second@(Triple{object : ma, supports: qsupports, dependencies: qdependencies}) <- q mo
    _ <- liftEff (addDependency first (TripleRef{subject: id, predicate: name }))
    _ <- liftEff (addDependency second (TripleRef{subject: id, predicate: name }))
    -- _ <- liftEff (addTriple1 id name ma [] [])
    pure (Triple{ subject: id,
      predicate: name
      , object: ma
      , supports: []
      , dependencies: []})
  combination Nothing = pure (Triple{ subject: ""
          , predicate: name
          , object: Nothing
          , supports: []
          , dependencies: []
          })

  name :: String
  name = "(" <>  nameOfp <> " >-> " <> nameOfq <> ")"

type Magic e =  (gm :: GLOBALMAP, avar :: AVAR, ajax :: AJAX, prd :: PROPDEFS, st :: (ST ResourceIndex), td :: THEORYDELTA | e)

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
