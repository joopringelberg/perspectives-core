module Perspectives.QueryCombinators where

import Prelude
import Data.Array (cons, elemIndex, nub, union) as Arr
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perspectives.Property (AsyncPropDefs, PluralGetter, SingleGetter)
import Perspectives.ResourceTypes (Resource)

-- | Compute the transitive closure of the SingleGetter to obtain a PluralGetter. NB: only for Resource results!
mclosure :: SingleGetter Resource -> PluralGetter Resource
mclosure f rs = closure' rs [] where
  closure' :: forall e. Resource -> Array Resource -> AsyncPropDefs e (Array Resource)
  closure' r ts = do
    (t :: Maybe Resource) <- f r
    case t of
      Nothing -> pure ts
      (Just x) ->
        case Arr.elemIndex (x :: Resource) ts of
          Nothing -> closure' x (Arr.cons x ts)
          _ -> pure ts

-- | Compute the transitive closure of the PluralGetter to obtain a PluralGetter. NB: only for Resource results!
aclosure :: PluralGetter Resource -> PluralGetter Resource
aclosure f r = do
  (t :: Array Resource) <- f r
  (x :: Array (Array Resource)) <- traverse (aclosure f) t
  (y :: Array Resource) <- pure $ join x
  pure $ Arr.nub (Arr.union t y)

-- | Concatenate the results of two PluralGetters.
concat :: forall a. Eq a => PluralGetter a -> PluralGetter a -> PluralGetter a
concat f g r = do
  a <- f r
  b <- g r
  pure $ Arr.union a b

-- | Add the result of a SingleGetter to that of a PluralGetter.
cons :: forall a. Eq a => SingleGetter a -> PluralGetter a -> PluralGetter a
cons f g r = do
  (a :: Maybe a) <- f r
  (b :: Array a) <- g r
  case a of
    Nothing -> pure b
    (Just el) ->
      case Arr.elemIndex el b of
        Nothing -> pure $ Arr.cons el b
        _ -> pure b

-- | Identity function: from a Resource a, compute Aff e (Just a) 
identity :: SingleGetter Resource
identity = pure <<< Just
