module Perspectives.QueryCombinatorsStudy where

import Prelude
import Data.Array (foldr, cons, elemIndex, nub, union) as Arr
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Perspectives.Location (Location, functionName, saveInLocation, locationValue, nameFunction, nestLocationInMonad)
import Perspectives.LocationT (LocationT(..))
import Perspectives.Property (AsyncPropDefsM, MemorizingPluralGetter, MemorizingSingleGetter, NestedLocation, PluralGetter, SingleGetter, StackedLocation, StackedMemorizingSingleGetter, StackedMemorizingPluralGetter)
import Perspectives.PropertyComposition (locationToStackedLocation, nestedToStackedLocation, stackedToNestedLocation, (>>->>))
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

-- This version works with StackedLocation and needs no low level Location functions!
mclosure :: StackedMemorizingSingleGetter Resource -> StackedMemorizingPluralGetter Resource
mclosure fun = nameFunction queryName (mclosure' fun []) where
  queryName :: String
  queryName = "mclosure_" <> functionName fun

  -- TODO. (h >>> nestedToStackedLocation) is anoniem.

  mclosure' :: StackedMemorizingSingleGetter Resource -> Array Resource -> StackedMemorizingPluralGetter Resource
  mclosure' f acc (loca :: Maybe Resource) =
    (nameFunction "cons" (maybe id Arr.cons)) <$> pure loca <*> (bind (pure loca) h)
    where
      h :: forall e. Maybe Resource -> StackedLocation e (Array Resource)
      h mr = case mr of
          (Just _) -> do
            (nextLoc :: Maybe Resource) <- f loca
            case nextLoc of
              Nothing -> mclosure' f acc nextLoc
              (Just nexta) -> let acc' = (maybe id Arr.cons) loca acc
                              in case Arr.elemIndex nexta acc' of
                                  Nothing -> mclosure' f acc' nextLoc
                                  otherwise -> pure []
          Nothing -> pure []

aclosure' :: PluralGetter Resource -> PluralGetter Resource
aclosure' f r = do
  (t :: Array Resource) <- f r
  (childClosures :: Array (Array Resource)) <- traverse (aclosure' f) (map Just t)
  pure $ Arr.nub (join (Arr.cons t childClosures))

-- This version will work as soon as >>->> works for StackedMemorizingPluralGetter's!
-- aclosure :: StackedMemorizingPluralGetter Resource -> StackedMemorizingPluralGetter Resource
-- aclosure f = (f >>->> (aclosure f))


-- | Concatenate the results of two PluralGetters.
concat' :: forall a. Eq a => PluralGetter a -> PluralGetter a -> PluralGetter a
concat' f g r = do
  a <- f r
  b <- g r
  pure $ Arr.union a b

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

-- | Identity function: from a Resource a, compute Aff e (Just a)
identity' :: SingleGetter Resource
identity' = pure

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

hasValue' :: forall a. SingleGetter a -> SingleGetter Boolean
hasValue' f r = do
  (v :: Maybe a) <- f r
  pure $ Just $ (maybe false (const true)) v

isNothing :: forall e a. Location (Maybe a) -> NestedLocation e (Maybe Boolean)
isNothing locr = pure $ map isNothing_ locr
    where
      isNothing_ :: (Maybe a) -> Maybe Boolean
      isNothing_ = Just <<< (maybe false (const true))
