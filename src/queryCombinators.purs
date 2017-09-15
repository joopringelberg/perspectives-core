module Perspectives.QueryCombinators where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Data.Array (foldr, cons, elemIndex, nub, union) as Arr
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Perspectives.Location (Location, locate, locationValue, memoizeMonadicFunction, nameFunction)
import Perspectives.Property (AsyncPropDefsM, MemoizingSingleGetter, PluralGetter, SingleGetter, MemoizingPluralGetter)
import Perspectives.Resource (locationFromResource)
import Perspectives.ResourceTypes (Resource)

-- | Compute the transitive closure of the MemoizingSingleGetter to obtain a MemoizingPluralGetter. NB: only for Resource results!
-- | TODO: memoiseert nog niet de recursieve stappen!
mclosure :: MemoizingSingleGetter Resource -> MemoizingPluralGetter Resource
mclosure fun res = closure fun (locate []) res where
  closure :: MemoizingSingleGetter Resource -> Location (Array Resource) -> MemoizingPluralGetter Resource
  closure f acc rs =
    do
      (x :: Location (Maybe Resource)) <- f rs
      case locationValue x of
        Nothing -> pure acc
        (Just r) -> case Arr.elemIndex r (locationValue acc) of
          Nothing -> closure f ((maybe id Arr.cons) <$> x <*> acc) x
          otherwise -> pure acc

aclosure' :: PluralGetter Resource -> PluralGetter Resource
aclosure' f r = do
  (t :: Array Resource) <- f r
  (childClosures :: Array (Array Resource)) <- traverse (aclosure' f) t
  pure $ Arr.nub (join (Arr.cons t childClosures))

-- | Compute the transitive closure of the MemoizingPluralGetter to obtain a PluralGetter. NB: only for Resource results!
-- | TODO: memoiseert nog niet de recursieve stappen!
aclosure :: MemoizingPluralGetter Resource -> MemoizingPluralGetter Resource
aclosure f (r :: Location (Maybe Resource)) =
    do
      (t :: Location (Array Resource)) <- f r
      (childClosures :: Array (Location (Array Resource))) <- (sequence (map g (locationValue t)))
      pure (locate (Arr.nub (join (Arr.cons (locationValue t) (map locationValue childClosures)))))
      where
        g :: forall e. Resource -> AsyncPropDefsM e (Location (Array Resource))
        g = ((liftEff <<< locationFromResource) >=> (aclosure f))

-- | Concatenate the results of two PluralGetters.
concat' :: forall a. Eq a => PluralGetter a -> PluralGetter a -> PluralGetter a
concat' f g r = do
  a <- f r
  b <- g r
  pure $ Arr.union a b

concat :: forall a. Eq a => MemoizingPluralGetter a -> MemoizingPluralGetter a -> MemoizingPluralGetter a
concat f g r = do
  a <- f r
  b <- g r
  pure $ Arr.union <$> a <*> b

-- | Add the result of a SingleGetter to that of a PluralGetter.
cons' :: forall a. Eq a => SingleGetter a -> PluralGetter a -> PluralGetter a
cons' f g r = do
  (a :: Maybe a) <- f r
  (b :: Array a) <- g r
  case a of
    Nothing -> pure b
    (Just el) ->
      case Arr.elemIndex el b of
        Nothing -> pure $ Arr.cons el b
        _ -> pure b

cons :: forall a. Eq a => MemoizingSingleGetter a -> MemoizingPluralGetter a -> MemoizingPluralGetter a
cons f g r = do
  (a :: Location (Maybe a)) <- f r
  (b :: Location (Array a)) <- g r
  case locationValue a of
    Nothing -> pure b
    (Just el) ->
      case Arr.elemIndex el (locationValue b) of
        Nothing -> pure $ (maybe id Arr.cons) <$> a <*> b
        _ -> pure b

-- | Identity function: from a Resource a, compute Aff e (Just a)
identity' :: SingleGetter Resource
identity' = pure <<< Just

identity :: MemoizingSingleGetter Resource
identity = pure

filter :: SingleGetter Boolean -> PluralGetter Resource -> PluralGetter Resource
filter c rs r = do
  (candidates :: Array Resource) <- rs r
  (judgedCandidates :: Array (Tuple Resource (Maybe Boolean))) <- traverse judge candidates
  pure (Arr.foldr takeOrDrop [] judgedCandidates)
  where
    judge :: forall e. Resource -> (AsyncPropDefsM e) (Tuple Resource (Maybe Boolean))
    judge candidate = do
      (judgement :: Maybe Boolean) <- c candidate
      pure (Tuple candidate judgement)
    takeOrDrop :: Tuple Resource (Maybe Boolean) -> Array Resource -> Array Resource
    takeOrDrop (Tuple res b) cumulator = case b of
      (Just true) -> Arr.cons res cumulator
      _ -> cumulator

hasValue' :: forall a. SingleGetter a -> SingleGetter Boolean
hasValue' f r = do
  (v :: Maybe a) <- f r
  pure $ Just $ (maybe false (const true)) v

hasValue :: forall a. MemoizingSingleGetter a -> MemoizingSingleGetter Boolean
hasValue f = memoizeMonadicFunction $ nameFunction "hasValue" g where
  g r = do
    (v :: Location (Maybe a)) <- f r
    pure $ locate (Just $ (maybe false (const true)) (locationValue v))
