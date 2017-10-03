module Perspectives.QueryCombinators where

import Prelude
import Data.Array (head, null, tail)
import Data.Array (cons, elemIndex, union) as Arr
import Data.Maybe (Maybe(..), maybe)
import Perspectives.Location (Location, functionName, nameFunction)
import Perspectives.Property (NestedLocation, StackedMemorizingPluralGetter, StackedMemorizingSingleGetter, StackedLocation)
import Perspectives.PropertyComposition (nestedToStackedLocation, stackedToNestedLocation, mcons)
import Perspectives.Resource (locationFromMaybeResource)
import Perspectives.ResourceTypes (Resource)

mclosure :: StackedMemorizingSingleGetter Resource -> String -> StackedMemorizingPluralGetter Resource
mclosure fun queryName = nameFunction queryName (mclosure' fun []) where
  mclosure' :: StackedMemorizingSingleGetter Resource -> Array Resource -> StackedMemorizingPluralGetter Resource
  mclosure' f acc mr@(Just r) | (maybe true (const false)) (Arr.elemIndex r acc) =
    mcons
      <$> nestedToStackedLocation (locationFromMaybeResource mr)
      <*> (f mr >>= (\next -> mclosure' f (Arr.cons r acc) next))
  mclosure' f acc otherwise = pure []

aclosure :: StackedMemorizingPluralGetter Resource -> String -> StackedMemorizingPluralGetter Resource
aclosure f queryName r = nameFunction queryName $ (f r) >>= ((pure <<< Just) >=> aclosure') where
  aclosure' :: forall e. Maybe (Array Resource) -> StackedLocation e (Array Resource)
  aclosure' (Just rs) | not $ null rs =
    union
      <$> aclosure f queryName (head rs)
      <*> (mcons <$> nestedToStackedLocation (locationFromMaybeResource (head rs)) <*> aclosure' (tail rs))
    where union a b = Arr.union a b
  aclosure' rs | otherwise = pure []

-- | This function memorizes due to LocationT apply.
concat :: forall a. Eq a => StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
concat f g = nameFunction queryName query
  where
    queryName = ("concat " <> functionName f <> " " <> functionName g)

    query r = Arr.union <$> f r <*> g r

-- | This function memorizes due to LocationT apply.
addTo :: forall a. Eq a => StackedMemorizingSingleGetter a -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
addTo f g = nameFunction queryName (query f g)
  where
    queryName = ("addTo " <> functionName f <> " " <> functionName g)

    query :: StackedMemorizingSingleGetter a -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
    query f' g' r = mcons <$> f' r <*> g' r

identity :: StackedMemorizingSingleGetter Resource
identity = nameFunction "identity" (\x -> nestedToStackedLocation (locationFromMaybeResource x))

filter :: StackedMemorizingSingleGetter Boolean -> String -> StackedMemorizingPluralGetter Resource -> StackedMemorizingPluralGetter Resource
filter c queryName rs =
  nameFunction queryName (rs >=> (nameFunction queryName ((pure <<< Just) >=> nameFunction queryName filterWithCriterium)))
  where
    filterWithCriterium :: forall e. Maybe (Array Resource) -> StackedLocation e (Array Resource)
    filterWithCriterium (Just candidates) | not $ null candidates =
      mcons <$> ((c $ head candidates) >>= addOrNot) <*> (filterWithCriterium $ tail candidates)
        where
        addOrNot mb = case mb of
          (Just true) -> pure $ head candidates
          otherwise -> pure Nothing
    filterWithCriterium otherwise = pure []


-- | This function is more general than a SingleGetter Boolean, because it will work on other arguments
-- | than just Maybe Resource.
-- |
-- | In this definition, the purescript compiler replaces a local isNothing with
-- | "isNothing1". That is no problem. Both the local function and the function that
-- | is generated for isNothing have a name. Explicit naming is therefore not necessary.
isNothing :: forall e a. Location (Maybe a) -> NestedLocation e (Maybe Boolean)
isNothing locr = pure $ map isNothing_ locr
    where
      isNothing_ :: (Maybe a) -> Maybe Boolean
      isNothing_ = Just <<< (maybe false (const true))

-- | The generated function will have the specialized name "hasValue <name of f>".
hasValue :: forall a. StackedMemorizingSingleGetter a -> StackedMemorizingSingleGetter Boolean
hasValue f = nameFunction ("hasValue " <> functionName f) (((f >>> stackedToNestedLocation) >=> isNothing) >>> nestedToStackedLocation)
