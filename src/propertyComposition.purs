module Perspectives.PropertyComposition where

import Control.Bind ((>=>))
import Control.Monad.Eff.Class (liftEff)
import Data.Array (cons, foldr, nub)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Perspectives.Location (Location, functionName, locationValue, nameFunction, traverseLoc)
import Perspectives.Property (MemorizingPluralGetter, MemorizingSingleGetter, PluralGetter, SingleGetter, AsyncPropDefsM)
import Perspectives.Resource (locationFromResource)
import Perspectives.ResourceTypes (Resource)
import Prelude (bind, id, join, pure, ($), map, (<<<), (>>>))

-- | Prefix a query that starts with a SingleGetter with this function
-- memorizeSingleGetter :: forall k l n. Monad n =>
--   (k -> n (Maybe l))
--   -> (Location (Maybe k) -> n (Location (Maybe l)))
memorizeSingleGetter :: forall a. SingleGetter a -> MemorizingSingleGetter a
memorizeSingleGetter g = nameFunction name (traverseLoc (nameFunction name (maybe (pure Nothing) g)))
  where name = (functionName g)

infixl 0 memorizeSingleGetter as |->

-- | prefix a query that starts with a PluralGetter with this function
-- memorizePluralGetter :: forall k l n. Monad n =>
--   (k -> n (Array l))
--   -> (Location (Maybe k) -> n (Location (Array l)))
memorizePluralGetter :: forall a. PluralGetter a -> MemorizingPluralGetter a
-- memorizePluralGetter g = nameFunction (functionName g) (traverseLoc (maybe (pure []) g)) -- DIT IS FOUT
memorizePluralGetter g = traverseLoc (nameFunction (functionName g) (maybe (pure []) g))

infixl 0 memorizePluralGetter as |->>

-- sTos :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Maybe c)))
sTos :: forall a. MemorizingSingleGetter Resource -> MemorizingSingleGetter a -> MemorizingSingleGetter a
sTos p q = p >=> q

infixl 0 sTos as >->

-- sTop :: forall a b c m. Monad m =>
--   (Location (Maybe a) -> m (Location (Maybe b)))
--   -> (b -> m (Array c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
sTop :: forall a. MemorizingSingleGetter Resource -> MemorizingPluralGetter a -> MemorizingPluralGetter a
sTop p q = p >=> q

infixl 0 sTop as >->>

-- pTos :: forall a b c m.  Monad m => Eq c =>
--   (Location (Maybe a) -> m (Location (Array b)))
--   -> (b -> m (Maybe c))
--   -> (Location (Maybe a) -> m (Location (Array c)))
pTos :: forall a. Eq a => MemorizingPluralGetter Resource -> MemorizingSingleGetter a -> MemorizingPluralGetter a
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
pTop :: forall a. Eq a => MemorizingPluralGetter Resource -> MemorizingPluralGetter a -> MemorizingPluralGetter a
pTop f g =
  let
    h :: forall e. Array Resource -> AsyncPropDefsM e (Array a)
    h arrayB = do
                  (y :: Array (Location (Array a))) <- traverse ((liftEff <<< locationFromResource) >=> g) arrayB
                  pure $ nub (join (map locationValue y))
  in f >=> (traverseLoc (nameFunction (functionName g) h))
  -- f >=> (traverseLoc (nameFunction (functionName g) (traverse ((liftEff <<< locationFromResource) >=> g) >=> (pure <<< nub <<< join <<< map locationValue))))

infixl 0 pTop as >>->>
