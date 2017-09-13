module Perspectives.PropertyComposition where

import Control.Bind ((>=>))
import Control.Monad (class Monad)
import Data.Array (cons, foldr, nub)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Location (Location, functionName, locationValue, nameFunction, setLocationValue, traverseLoc)
import Prelude (bind, id, join, pure, ($), (*>), (>>>))

-- | prefix a query that starts with a SingleGetter with this function
liftSingleGetter :: forall k l n. Monad n =>
  (k -> n (Maybe l))
  -> (Location (Maybe k) -> n (Location (Maybe l)))
liftSingleGetter g aloc = traverseLoc (nameFunction (functionName g) (maybe (pure Nothing) g)) aloc

infix 0 liftSingleGetter as |->

-- | prefix a query that starts with a PluralGetter with this function
liftPluralGetter :: forall k l n. Monad n =>
  (k -> n (Array l))
  -> (Location (Maybe k) -> n (Location (Array l)))
liftPluralGetter g aloc = traverseLoc (nameFunction (functionName g) (maybe (pure []) g)) aloc

infix 0 liftPluralGetter as |->>

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
sTos p q = p >=> (liftSingleGetter q)

infix 0 sTos as >->

sTop :: forall a b c m. Monad m =>
  (Location (Maybe a) -> m (Location (Maybe b)))
  -> (b -> m (Array c))
  -> (Location (Maybe a) -> m (Location (Array c)))
sTop p q = p >=> (liftPluralGetter q)

infix 0 sTop as >->>

-- sTop f g a = do
--   x <- f a
--   case (x :: Maybe b) of
--     Nothing -> pure []
--     (Just b) -> g b
-- sTop f g = f >=> (maybe (pure []) g)

pTos :: forall a b c m.  Monad m => Eq c =>
  (Location (Maybe a) -> m (Location (Array b)))
  -> (b -> m (Maybe c))
  -> (Location (Maybe a) -> m (Location (Array c)))
pTos f g a =
  let
    h :: Array b -> m (Array c)
    h arrayB = do
                (p :: Array (Maybe c)) <- traverse g arrayB
                pure $ nub $ foldr (maybe id cons) [] p
  in
    do
      (x :: Location (Array b)) <- f (a :: Location (Maybe a))
      traverseLoc (nameFunction (functionName g) h) x


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
