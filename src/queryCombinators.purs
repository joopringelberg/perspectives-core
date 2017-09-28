module Perspectives.QueryCombinators where

import Prelude
import Data.Array (foldr, cons, elemIndex, nub, union) as Arr
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Perspectives.Location (Location, functionName, locationValue, nameFunction, nestLocationInMonad)
import Perspectives.Property (AsyncPropDefsM, MemorizingPluralGetter, MemorizingSingleGetter, NestedLocation, StackedMemorizingSingleGetter, StackedMemorizingPluralGetter)
import Perspectives.PropertyComposition (memorizeInStackedLocation, nestedToStackedLocation, stackedToNestedLocation, (>>->>))
import Perspectives.Resource (locationFromResource)
import Perspectives.ResourceTypes (Resource)

mclosure :: StackedMemorizingSingleGetter Resource -> StackedMemorizingPluralGetter Resource
mclosure fun = nameFunction queryName (mclosure' fun []) where
  queryName :: String
  queryName = "mclosure_" <> functionName fun

  mclosure' :: StackedMemorizingSingleGetter Resource -> Array Resource -> StackedMemorizingPluralGetter Resource
  mclosure' f acc Nothing = pure []
  mclosure' f acc (Just r) | (maybe false (const true)) (Arr.elemIndex r acc) = pure []
  mclosure' f acc mr@(Just r) =
      do
        next <- f mr
        rest <- mclosure' f (Arr.cons r acc) next
        pure $ (maybe id Arr.cons) next rest

-- | Compute the transitive closure of the MemorizingPluralGetter to obtain a PluralGetter. NB: only for Resource results!
aclosure :: StackedMemorizingPluralGetter Resource -> StackedMemorizingPluralGetter Resource
aclosure f = (f >>->> (aclosure f))

-- | This function memorizes due to LocationT apply.
concat :: forall a. Eq a => MemorizingPluralGetter a -> MemorizingPluralGetter a -> MemorizingPluralGetter a
concat f g = nameFunction queryName query
  where
    queryName = ("concat " <> functionName f <> " " <> functionName g)

    -- NOTE we do not want to rename Arr.union itself.
    aux = nameFunction "union" (\a b -> Arr.union a b)

    query r = stackedToNestedLocation $ aux <$> (nestedToStackedLocation <<< f) r <*> (nestedToStackedLocation <<< g) r

-- | This function memorizes due to LocationT apply.
addTo :: forall a. Eq a => StackedMemorizingSingleGetter a -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
addTo f g = nameFunction queryName (query f g)
  where
    queryName = ("addTo " <> functionName f <> " " <> functionName g)

    mcons :: (Maybe a) -> (Array a) -> (Array a)
    mcons = maybe id Arr.cons

    query :: StackedMemorizingSingleGetter a -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
    query f' g' r = mcons <$> f' r <*> g' r

identity :: StackedMemorizingSingleGetter Resource
identity = memorizeInStackedLocation (nameFunction "identity" (\x -> pure x))

-- | This function memorizes due to nestLocationInMonad.
filter :: MemorizingSingleGetter Boolean -> MemorizingPluralGetter Resource -> MemorizingPluralGetter Resource
filter c rs=
  nameFunction name (rs >=> (nestLocationInMonad (nameFunction name filterWithCriterium)))
  where
    name :: String
    name = "filter " <> (functionName c)

    filterWithCriterium :: forall e. Array Resource -> AsyncPropDefsM e (Array Resource)
    filterWithCriterium candidates = do
      (judgedCandidates :: Array (Tuple Resource (Maybe Boolean))) <- traverse judge candidates
      pure $ (Arr.foldr takeOrDrop [] judgedCandidates)

    judge :: forall e. Resource -> (AsyncPropDefsM e) (Tuple Resource (Maybe Boolean))
    judge candidate = do
      (res :: Location (Maybe Resource)) <- locationFromResource candidate
      (judgement :: Location (Maybe Boolean)) <- c res
      pure (Tuple candidate (locationValue judgement))

    takeOrDrop :: Tuple Resource (Maybe Boolean) -> Array Resource -> Array Resource
    takeOrDrop (Tuple res b) cumulator = case b of
      (Just true) -> Arr.cons res cumulator
      _ -> cumulator

-- | In this definition, the purescript compiler replaces a local isNothing with
-- | "isNothing1". That is no problem. Both the local function and the function that
-- | is generated for isNothing have a name. Explicit naming is therefore not necessary.
isNothing :: forall e a. Location (Maybe a) -> NestedLocation e (Maybe Boolean)
isNothing locr = pure $ map isNothing_ locr
    where
      isNothing_ :: (Maybe a) -> Maybe Boolean
      isNothing_ = Just <<< (maybe false (const true))

-- | The generated function will have the specialized name "hasValue <name of f>".
hasValue :: forall a. MemorizingSingleGetter a -> MemorizingSingleGetter Boolean
hasValue f = nameFunction ("hasValue " <> functionName f) (f >=> isNothing)
