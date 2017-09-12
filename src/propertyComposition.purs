module Perspectives.PropertyComposition where

import Control.Bind ((>=>))
import Control.Monad (class Monad)
import Data.Array (cons, foldr, nub)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Location (Location, connectLocations, locate, locationValue, traverseLoc)
import Prelude (bind, id, join, pure, ($))

-- | Start a query with this function
query :: forall k l n. Monad n =>
  (k -> n (Maybe l))
  -> (Location (Maybe k) -> n (Location (Maybe l)))
query g aloc = traverseLoc (maybe (pure Nothing) g) aloc

-- sTos :: forall a b c m. Monad m =>
--   (a -> m (Maybe b))
--   -> (b -> m (Maybe c))
--   -> a
--   -> m (Maybe c)
-- sTos f g = f >=> (maybe (pure Nothing) g)

sTos :: forall a b c m. Monad m =>
  (Location (Maybe a) -> m (Location (Maybe b)))
  -> (b -> m (Maybe c))
  -> (Location (Maybe a) -> m (Location (Maybe c)))
sTos p q =
  p >=> (lift q)
  where
    lift :: forall k l n. Monad n =>
      (k -> n (Maybe l))
      -> (Location (Maybe k) -> n (Location (Maybe l)))
    lift g aloc = traverseLoc (maybe (pure Nothing) g) aloc

    lift' :: forall k l n. Monad n =>
      (k -> n (Maybe l))
      -> (Location (Maybe k) -> n (Location (Maybe l)))
    lift' g aLoc = case locationValue aLoc of
      Nothing -> pure $ locate Nothing
      (Just a) -> do
        maybeB <- g a
        case maybeB of
          Nothing -> pure (locate Nothing)
          justB -> pure (connectLocations aLoc g (locate justB))

-- traverse :: forall a b m. Applicative m => (a -> m b) -> t a -> m (t b)
-- m = n
-- t = Location
-- a = k
-- b = Maybe l
-- traverse :: forall k l n. Applicative n =>
--  (k -> n (Maybe l))
--    -> Location k -> n (Location (Maybe l))

-- m = n
-- t = Location
-- a = Maybe k
-- b = Maybe l
-- traverse :: forall k l n. Applicative n =>
--  (Maybe k -> n (Maybe l))
--    -> Location (Maybe k) -> n (Location (Maybe l))
--
-- Now transform:
-- k -> n (Maybe l)
-- into:
-- Maybe k -> n (Maybe l)
-- with:
-- maybe (pure Nothing) g

infix 0 sTos as >->

sTop :: forall a b c m. Monad m =>
  (a -> m (Maybe b))
  -> (b -> m (Array c))
  -> a
  -> m (Array c)
sTop f g a = do
  x <- f a
  case (x :: Maybe b) of
    Nothing -> pure []
    (Just b) -> g b
-- sTop f g = f >=> (maybe (pure []) g)

infix 0 sTop as >->>

pTos :: forall a b c m.  Monad m => Eq c =>
  (a -> m (Array b))
  -> (b -> m (Maybe c))
  -> a
  -> m (Array c)
pTos f g a = do
  x <- f a
  y <- traverse g (x :: Array b)
  pure $ nub $ foldr (maybe id cons) [] (y :: Array (Maybe c))

infix 0 pTos as >>->

pTop :: forall a b c m.  Monad m => Eq c =>
  (a -> m (Array b))
  -> (b -> m (Array c))
  -> a
  -> m (Array c)
pTop f g a = do
  x <- f a
  y <- traverse g (x :: Array b)
  pure $ nub (join (y :: Array (Array c)))
-- xTox f g = f >=> traverse g >=> (pure <<< nub <<< join)

infix 0 pTop as >>->>
