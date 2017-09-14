module Perspectives.PropertyComposition where

import Control.Bind ((>=>))
import Data.Array (cons, foldr, nub)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Location (functionName, nameFunction, traverseLoc)
import Perspectives.Property (MemoizingPluralGetter, MemoizingSingleGetter, PluralGetter, SingleGetter, AsyncPropDefsM)
import Perspectives.ResourceTypes (Resource)
import Prelude (bind, id, join, pure, ($))

-- | Prefix a query that starts with a SingleGetter with this function
-- liftSingleGetter :: forall k l n. Monad n =>
--   (k -> n (Maybe l))
--   -> (Location (Maybe k) -> n (Location (Maybe l)))
liftSingleGetter :: forall a. SingleGetter a -> MemoizingSingleGetter a
liftSingleGetter g = traverseLoc (nameFunction (functionName g) (maybe (pure Nothing) g))

infixl 0 liftSingleGetter as |->

-- | prefix a query that starts with a PluralGetter with this function
-- liftPluralGetter :: forall k l n. Monad n =>
--   (k -> n (Array l))
--   -> (Location (Maybe k) -> n (Location (Array l)))
liftPluralGetter :: forall a. PluralGetter a -> MemoizingPluralGetter a
liftPluralGetter g = traverseLoc (nameFunction (functionName g) (maybe (pure []) g))

infixl 0 liftPluralGetter as |->>

-- sTos :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Maybe c)))
sTos :: forall a. MemoizingSingleGetter Resource -> SingleGetter a -> MemoizingSingleGetter a
sTos p q = p >=> (liftSingleGetter q)

infixl 0 sTos as >->

-- sTop :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
sTop :: forall a. MemoizingSingleGetter Resource -> PluralGetter a -> MemoizingPluralGetter a
sTop p q = p >=> (liftPluralGetter q)

infixl 0 sTop as >->>

-- pTos :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
pTos :: forall a. Eq a => MemoizingPluralGetter Resource -> SingleGetter a -> MemoizingPluralGetter a
pTos f g =
  let
    h :: forall e. Array Resource -> AsyncPropDefsM e (Array a)
    h arrayB = do
                (p :: Array (Maybe a)) <- traverse g arrayB
                pure $ nub $ foldr (maybe id cons) [] p
    in f >=> (traverseLoc (nameFunction (functionName g) h))
  -- f >=> (traverseLoc (nameFunction (functionName g) (traverse g >=> (pure <<< nub <<< (foldr (maybe id cons) [])))))

infixl 0 pTos as >>->

-- pTop :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
pTop :: forall a. Eq a => MemoizingPluralGetter Resource -> PluralGetter a -> MemoizingPluralGetter a
pTop f g =
  let
    h :: forall e. Array Resource -> AsyncPropDefsM e (Array a)
    h arrayB = do
                  y <- traverse g (arrayB :: Array Resource)
                  pure $ nub (join (y :: Array (Array a)))
  in f >=> (traverseLoc (nameFunction (functionName g) h))
  -- f >=> (traverseLoc (nameFunction (functionName g) (traverse g >=> (pure <<< nub <<< join))))

infixl 0 pTop as >>->>
