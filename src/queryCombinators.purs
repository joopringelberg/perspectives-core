module Perspectives.QueryCombinators where

import Prelude
import Data.Array (cons, elemIndex, nub, union)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Perspectives.Property (AsyncPropDefs, PluralGetter, SingleGetter)
import Perspectives.ResourceTypes (Resource)

mclosure :: SingleGetter Resource -> PluralGetter Resource
mclosure f rs = closure' rs [] where
  closure' :: forall e. Resource -> Array Resource -> AsyncPropDefs e (Array Resource)
  closure' r ts = do
    (t :: Maybe Resource) <- f r
    case t of
      Nothing -> pure ts
      (Just x) ->
        case elemIndex (x :: Resource) ts of
          Nothing -> closure' x (cons x ts)
          _ -> pure ts

aclosure :: PluralGetter Resource -> PluralGetter Resource
aclosure f r = do
  (t :: Array Resource) <- f r
  (x :: Array (Array Resource)) <- traverse (aclosure f) t
  (y :: Array Resource) <- pure $ join x
  pure $ nub (union t y)

--traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
-- a = Resource
-- m = AsyncPropDefs e
-- b = Array Resource
-- t = Array
-- Resource -> AsyncPropDefs (Array Resource)) -> Array Resource -> AsyncPropDefs e (Array (Array Resource))
