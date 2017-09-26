module Perspectives.QueryCombinators where

import Prelude
import Data.Array (foldr, cons, elemIndex, nub, union) as Arr
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Perspectives.Location (Location, functionName, saveInLocation, locationValue, nameFunction, nestLocationInMonad)
import Perspectives.LocationT (LocationT(..))
import Perspectives.Property (AsyncPropDefsM, MemorizingPluralGetter, MemorizingSingleGetter, NestedLocation, PluralGetter, SingleGetter, StackedLocation)
import Perspectives.PropertyComposition (locationToStackedLocation, nestedToStackedLocation, stackedToNestedLocation)
import Perspectives.Resource (locationFromResource)
import Perspectives.ResourceTypes (Resource, LocationWithResource)

close :: forall a.
  (Location (Maybe a) -> Location (Maybe a))
  -> Location (Maybe a)
  -> Location (Array a)
close f loca = (maybe id Arr.cons) <$> loca <*> (bind loca h)
  where
    h :: Maybe a -> Location (Array a)
    h mr = case mr of
      (Just _) -> close f (f loca)
      Nothing -> saveInLocation []

-- This is the same closure, but now typed as LocationT (AsyncPropDefsM e) (Maybe a). This means the Aff and
-- Location monads have been stacked.
close' :: MemorizingSingleGetter Resource -> MemorizingPluralGetter Resource
close' f (loca :: Location (Maybe Resource)) = stackedToNestedLocation ((maybe id Arr.cons) <$> liftedLoca <*> bind liftedLoca (nestedToStackedLocation <<< h))
  where

    liftedLoca :: forall e. StackedLocation e (Maybe Resource)
    liftedLoca = LocationT (pure loca)

    h :: forall e. Maybe Resource -> NestedLocation e (Array Resource)
    h mr = case mr of
      (Just _) -> do
        -- by lowering from LocationT, we obtain the Location in Aff.
        -- close expects a Location (because f does so).
        (x :: Location (Maybe Resource)) <- (f loca)
        close' f x
      Nothing -> pure $ saveInLocation []

-- Here we add an accumulator to the pure recursive function, in order to detect a value of a that has been seen before.
-- In this way we prevent endless recursion when f is idempotent.
closeWithAcc :: forall a. Eq a =>
  (Location (Maybe a) -> Location (Maybe a))
  -> Array a
  -> Location (Maybe a)
  -> Location (Array a)
closeWithAcc f acc loca = (maybe id Arr.cons) <$> loca <*> (bind loca h)
  where
    h :: Maybe a -> Location (Array a)
    h (Just a) =
      let
        nextLoc = f loca
        acc' = (maybe id Arr.cons) (locationValue loca) acc
      in case locationValue nextLoc of
        Nothing -> closeWithAcc f acc' nextLoc
        (Just next) ->
          case Arr.elemIndex next acc' of
            Nothing -> closeWithAcc f acc' nextLoc
            otherwise -> saveInLocation []
    h Nothing = saveInLocation []

-- | This function memorizes.
mclosure :: MemorizingSingleGetter Resource -> MemorizingPluralGetter Resource
mclosure fun = nameFunction ("mclosure_" <> functionName fun) (mclosure' fun []) where

  mclosure' :: MemorizingSingleGetter Resource -> Array Resource -> MemorizingPluralGetter Resource
  mclosure' f acc (loca :: LocationWithResource) =
  -- TODO. (h >>> nestedToStackedLocation) is anoniem.
    stackedToNestedLocation ((nameFunction "cons" (maybe id Arr.cons)) <$> stackedResource <*> (bind stackedResource (h >>> nestedToStackedLocation)))
    where

      stackedResource :: forall e. StackedLocation e (Maybe Resource)
      stackedResource = locationToStackedLocation loca

      h :: forall e. Maybe Resource -> NestedLocation e (Array Resource)
      h mr = case mr of
          (Just _) -> do
            (nextLoc :: Location (Maybe Resource)) <- (f loca)
            case locationValue nextLoc of
              Nothing -> mclosure' f acc nextLoc
              (Just nexta) -> let acc' = (maybe id Arr.cons) (locationValue loca) acc
                              in case Arr.elemIndex nexta acc' of
                                  Nothing -> mclosure' f acc' nextLoc
                                  otherwise -> pure $saveInLocation []
          Nothing -> pure $ saveInLocation []

aclosure' :: PluralGetter Resource -> PluralGetter Resource
aclosure' f r = do
  (t :: Array Resource) <- f r
  (childClosures :: Array (Array Resource)) <- traverse (aclosure' f) (map Just t)
  pure $ Arr.nub (join (Arr.cons t childClosures))

-- | Compute the transitive closure of the MemorizingPluralGetter to obtain a PluralGetter. NB: only for Resource results!
-- | TODO: memoiseert nog niet de recursieve stappen!
aclosure :: MemorizingPluralGetter Resource -> MemorizingPluralGetter Resource
aclosure f (r :: Location (Maybe Resource)) =
    do
      (t :: Location (Array Resource)) <- f r
      (childClosures :: Array (Location (Array Resource))) <- (traverse (locationFromResource >=> g) (locationValue t))
      pure (saveInLocation (Arr.nub (join (Arr.cons (locationValue t) (map locationValue childClosures)))))
      where
        g :: forall e. LocationWithResource -> AsyncPropDefsM e (Location (Array Resource))
        g = (aclosure f)

-- | Concatenate the results of two PluralGetters.
concat' :: forall a. Eq a => PluralGetter a -> PluralGetter a -> PluralGetter a
concat' f g r = do
  a <- f r
  b <- g r
  pure $ Arr.union a b

-- | This function memorizes due to LocationT apply.
concat :: forall a. Eq a => MemorizingPluralGetter a -> MemorizingPluralGetter a -> MemorizingPluralGetter a
concat f g = nameFunction queryName query
  where
    queryName = ("concat " <> functionName f <> " " <> functionName g)

    -- NOTE we do not want to rename Arr.union itself.
    aux = nameFunction "union" (\a b -> Arr.union a b)

    query r = stackedToNestedLocation $ aux <$> (nestedToStackedLocation <<< f) r <*> (nestedToStackedLocation <<< g) r

-- | Add the result of a SingleGetter to that of a PluralGetter.
addTo' :: forall a. Eq a => SingleGetter a -> PluralGetter a -> PluralGetter a
addTo' f g r = do
  (a :: Maybe a) <- f r
  (b :: Array a) <- g r
  case a of
    Nothing -> pure b
    (Just el) ->
      case Arr.elemIndex el b of
        Nothing -> pure $ Arr.cons el b
        _ -> pure b

-- | This function memorizes due to LocationT apply.
addTo :: forall a. Eq a => MemorizingSingleGetter a -> MemorizingPluralGetter a -> MemorizingPluralGetter a
addTo f' g' = nameFunction queryName (query f' g')
  where
    queryName = ("addTo " <> functionName f' <> " " <> functionName g' )

    aux :: (Maybe a) -> (Array a) -> (Array a)
    aux = nameFunction "cons" (maybe id Arr.cons)

    query :: MemorizingSingleGetter a -> MemorizingPluralGetter a -> MemorizingPluralGetter a
    query f g r = stackedToNestedLocation (aux <$> (nestedToStackedLocation <<< f) r <*> (nestedToStackedLocation <<< g) r)

-- | Identity function: from a Resource a, compute Aff e (Just a)
identity' :: SingleGetter Resource
identity' = pure

identity :: MemorizingSingleGetter Resource
identity = nestLocationInMonad (nameFunction "identity" (\x -> pure x))

filter' :: SingleGetter Boolean -> PluralGetter Resource -> PluralGetter Resource
filter' c rs (r :: Maybe Resource) = do
  (candidates :: Array Resource) <- rs r
  (judgedCandidates :: Array (Tuple Resource (Maybe Boolean))) <- traverse judge candidates
  pure (Arr.foldr takeOrDrop [] judgedCandidates)
  where
    judge :: forall e. Resource -> (AsyncPropDefsM e) (Tuple Resource (Maybe Boolean))
    judge candidate = do
      (judgement :: Maybe Boolean) <- c (Just candidate)
      pure (Tuple candidate judgement)
    takeOrDrop :: Tuple Resource (Maybe Boolean) -> Array Resource -> Array Resource
    takeOrDrop (Tuple res b) cumulator = case b of
      (Just true) -> Arr.cons res cumulator
      _ -> cumulator

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

hasValue' :: forall a. SingleGetter a -> SingleGetter Boolean
hasValue' f r = do
  (v :: Maybe a) <- f r
  pure $ Just $ (maybe false (const true)) v

-- | The generated function will have the specialized name "hasValue <name of f>".
hasValue :: forall a. MemorizingSingleGetter a -> MemorizingSingleGetter Boolean
hasValue f = nameFunction ("hasValue " <> functionName f) (f >=> isNothing)
