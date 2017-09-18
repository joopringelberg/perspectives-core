module Perspectives.PropertyComposition where

import Control.Bind ((>=>))
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, foldr, nub)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Location (Location, functionName, locationValue, nameFunction, traverseLoc)
import Perspectives.Property (AsyncPropDefsM, MemoizingPluralGetter, MemoizingSingleGetter, PluralGetter, SingleGetter, PropertyName)
import Perspectives.Resource (locationFromResource)
import Perspectives.ResourceTypes (Resource)
import Prelude (bind, id, join, pure, ($), map, (<<<), (>>>))

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
liftPluralGetter g = nameFunction (functionName g) (traverseLoc (maybe (pure []) g))
-- liftPluralGetter g = traverseLoc (nameFunction (functionName g) (maybe (pure []) g))

infixl 0 liftPluralGetter as |->>

-- sTos :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Maybe c)))
sTos :: forall a. MemoizingSingleGetter Resource -> MemoizingSingleGetter a -> MemoizingSingleGetter a
sTos p q = p >=> q

infixl 0 sTos as >->

-- sTop :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
sTop :: forall a. MemoizingSingleGetter Resource -> MemoizingPluralGetter a -> MemoizingPluralGetter a
sTop p q = p >=> q

infixl 0 sTop as >->>

-- pTos :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- TODO: de dependency tracking is niet voldoende. Verandering in de SingleGetter g leidt niet tot herberekening.
pTos :: forall a. Eq a => MemoizingPluralGetter Resource -> MemoizingSingleGetter a -> MemoizingPluralGetter a
pTos f g =
  let
    h :: forall e. Array Resource -> AsyncPropDefsM e (Array a)
    h arrayB = do
                (p :: Array (Location (Maybe a))) <- traverse ((liftEff <<< locationFromResource) >=> g) arrayB
                pure $ nub $ foldr (locationValue >>> (maybe id cons)) [] p
    in f >=> (traverseLoc (nameFunction (functionName g) h))
  -- f >=> (traverseLoc (nameFunction (functionName g) (traverse ((liftEff <<< locationFromResource) >=> g) >=> (pure <<< nub <<< (foldr (locationValue >>> (maybe id cons)) [])))))

infixl 0 pTos as >>->

-- pTop :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
-- TODO: de dependency tracking is niet voldoende. Verandering in de PluralGetter g leidt niet tot herberekening.
pTop :: forall a. Eq a => MemoizingPluralGetter Resource -> MemoizingPluralGetter a -> MemoizingPluralGetter a
pTop f g =
  let
    h :: forall e. Array Resource -> AsyncPropDefsM e (Array a)
    h arrayB = do
                  (y :: Array (Location (Array a))) <- traverse ((liftEff <<< locationFromResource) >=> g) arrayB
                  pure $ nub (join (map locationValue y))
  in f >=> (traverseLoc (nameFunction (functionName g) h))
  -- f >=> (traverseLoc (nameFunction (functionName g) (traverse ((liftEff <<< locationFromResource) >=> g) >=> (pure <<< nub <<< join <<< map locationValue))))

infixl 0 pTop as >>->>
