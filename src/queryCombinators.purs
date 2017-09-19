module Perspectives.QueryCombinators where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Data.Array (foldr, cons, elemIndex, nub, union) as Arr
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..))
import Perspectives.Location (Location, functionName, locate, locationValue, memorizeMonadicFunction, nameFunction)
import Perspectives.Property (AsyncPropDefsM, MemorizingSingleGetter, PluralGetter, SingleGetter, MemorizingPluralGetter)
import Perspectives.Resource (locationFromResource)
import Perspectives.ResourceTypes (Resource)

-- | Compute the transitive closure of the MemorizingSingleGetter to obtain a MemorizingPluralGetter. NB: only for Resource results!
-- | TODO: memoiseert nog niet de recursieve stappen!
mclosure :: MemorizingSingleGetter Resource -> MemorizingPluralGetter Resource
mclosure fun res = closure fun (locate []) res where
  closure :: MemorizingSingleGetter Resource -> Location (Array Resource) -> MemorizingPluralGetter Resource
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

-- | Compute the transitive closure of the MemorizingPluralGetter to obtain a PluralGetter. NB: only for Resource results!
-- | TODO: memoiseert nog niet de recursieve stappen!
aclosure :: MemorizingPluralGetter Resource -> MemorizingPluralGetter Resource
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

concat :: forall a. Eq a => MemorizingPluralGetter a -> MemorizingPluralGetter a -> MemorizingPluralGetter a
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

cons :: forall a. Eq a => MemorizingSingleGetter a -> MemorizingPluralGetter a -> MemorizingPluralGetter a
cons f' g' = nameFunction ("cons_" <> functionName f' <> "_" <> functionName g') aux f' g'
  where
    fname :: String
    fname = ("cons_" <> functionName f' <> "_" <> functionName g')
    aux :: MemorizingSingleGetter a -> MemorizingPluralGetter a -> MemorizingPluralGetter a
    aux f g r = do
      (a :: Location (Maybe a)) <- f r
      (b :: Location (Array a)) <- g r
      pure $ (nameFunction fname (maybe id Arr.cons)) <$> a <*> b

-- | Identity function: from a Resource a, compute Aff e (Just a)
identity' :: SingleGetter Resource
identity' = pure <<< Just

identity :: MemorizingSingleGetter Resource
identity = nameFunction "identity" pure

filter' :: SingleGetter Boolean -> PluralGetter Resource -> PluralGetter Resource
filter' c rs r = do
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

-- | TODO: filter memoiseert nog niet!
filter :: MemorizingSingleGetter Boolean -> MemorizingPluralGetter Resource -> MemorizingPluralGetter Resource
filter c rs r = do
  (candidates :: Location (Array Resource)) <- rs (r :: Location (Maybe Resource))
  (judgedCandidates :: Array (Maybe Resource)) <- traverse judge (locationValue candidates)
  pure $ locate (Arr.foldr (maybe id Arr.cons) [] judgedCandidates)
  where
    judge :: forall e. Resource -> (AsyncPropDefsM e) (Maybe Resource)
    judge candidate = do
      res <- liftEff $ locationFromResource candidate
      (judgement :: Location (Maybe Boolean)) <- c res
      pure $ (maybe Nothing (const (Just candidate))) (locationValue judgement)

hasValue' :: forall a. SingleGetter a -> SingleGetter Boolean
hasValue' f r = do
  (v :: Maybe a) <- f r
  pure $ Just $ (maybe false (const true)) v

-- | The generated function will have the specialized name "has_<name of f>".
hasValue :: forall a. MemorizingSingleGetter a -> MemorizingSingleGetter Boolean
hasValue f = nameFunction name (f >=> (pure <<< map (nameFunction name (Just <<< (maybe false (const true)))) ))
  where name = "has_" <> functionName f
