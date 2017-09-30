module Perspectives.QueryCombinators where

import Prelude
import Data.Array (foldr, cons, elemIndex, union, nub) as Arr
import Data.Eq ((==))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Perspectives.Location (Location, functionName, nameFunction, (>==>))
import Perspectives.Property (MemorizingPluralGetter, NestedLocation, StackedMemorizingPluralGetter, StackedMemorizingSingleGetter, StackedLocation)
import Perspectives.PropertyComposition (memorizeInStackedLocation, nestedToStackedLocation, stackedToNestedLocation)
import Perspectives.Resource (locationFromMaybeResource)
import Perspectives.ResourceTypes (Resource)

mclosure :: StackedMemorizingSingleGetter Resource -> StackedMemorizingPluralGetter Resource
mclosure fun = nameFunction queryName (mclosure' fun []) where
  queryName :: String
  queryName = "mclosure " <> functionName fun

  mclosure' :: StackedMemorizingSingleGetter Resource -> Array Resource -> StackedMemorizingPluralGetter Resource
  mclosure' f acc Nothing = pure []
  mclosure' f acc (Just r) | (maybe false (const true)) (Arr.elemIndex r acc) = pure []
  -- mclosure' f acc mr@(Just r) =
  --     do
  --       next <- f mr
  --       if next == mr then pure [] else do
  --         rest <- mclosure' f (Arr.cons r acc) next
  --         pure $ mcons next rest

  mclosure' f acc mr@(Just r) =
    (f >=> nameFunction ("mclosure " <> functionName f)
      (\next -> if next == mr then pure [] else do
        rest <- mclosure' f (Arr.cons r acc) next
        pure $ mcons next rest)) mr

mcons :: forall a. (Maybe a) -> (Array a) -> (Array a)
mcons = nameFunction "mcons" (maybe id Arr.cons)

-- | Compute the transitive closure of the MemorizingPluralGetter to obtain a PluralGetter. NB: only for Resource results!
aclosure :: StackedMemorizingPluralGetter Resource -> StackedMemorizingPluralGetter Resource
aclosure f' = nameFunction queryName (aclosure' f') where
  queryName :: String
  queryName = "aclosure " <> functionName f'

  aclosure' :: StackedMemorizingPluralGetter Resource -> StackedMemorizingPluralGetter Resource
  aclosure' f r = do
    (t :: Array Resource) <- f r
    (childClosures :: Array (Array Resource)) <- traverse (aclosure f) (map Just t)
    pure $ Arr.nub (join (Arr.cons t childClosures))

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

    query :: StackedMemorizingSingleGetter a -> StackedMemorizingPluralGetter a -> StackedMemorizingPluralGetter a
    query f' g' r = mcons <$> f' r <*> g' r

identity :: StackedMemorizingSingleGetter Resource
identity = nameFunction "identity" (\x -> nestedToStackedLocation (locationFromMaybeResource x))

filter :: StackedMemorizingSingleGetter Boolean -> StackedMemorizingPluralGetter Resource -> StackedMemorizingPluralGetter Resource
filter c rs =
  nameFunction name (rs >=> (nameFunction name filterWithCriterium))
  where
    name :: String
    name = "filter " <> (functionName c)

    filterWithCriterium :: forall e. Array Resource -> StackedLocation e (Array Resource)
    filterWithCriterium candidates = do
      (judgedCandidates :: Array (Tuple Resource (Maybe Boolean))) <- traverse judge candidates
      pure $ (Arr.foldr takeOrDrop [] judgedCandidates)

    judge :: forall e. Resource -> (StackedLocation e) (Tuple Resource (Maybe Boolean))
    judge candidate = do
      (judgement :: Maybe Boolean) <- c (Just candidate)
      pure (Tuple candidate judgement)

    takeOrDrop :: Tuple Resource (Maybe Boolean) -> Array Resource -> Array Resource
    takeOrDrop (Tuple res b) cumulator = case b of
      (Just true) -> Arr.cons res cumulator
      _ -> cumulator

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
