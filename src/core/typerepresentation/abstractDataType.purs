module Perspectives.Representation.ADT where

import Data.Array (elemIndex, filter, intersect, union)
import Data.Foldable (foldMap, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType)
import Prelude (class Eq, class Monad, class Show, bind, notEq, pure, ($))
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

data ADT a = ST a | SUM (Array (ADT a)) | PROD (Array (ADT a)) | NOTYPE
-- data CompoundRoleType

derive instance genericRepBinding :: Generic (ADT a) _

instance showBinding :: (Show a) => Show (ADT a) where
  show b = genericShow b

instance eqBinding :: (Eq a) => Eq (ADT a) where
  eq b1 b2 = genericEq b1 b2

instance writeForeignBinding :: (WriteForeign a) => WriteForeign (ADT a) where
  writeImpl b = unsafeToForeign (writeJSON b)

instance readForeignBinding :: (ReadForeign a) => ReadForeign (ADT a) where
  readImpl b = readJSON' (unsafeFromForeign b)

class Reducible a b where
  reduce :: forall m. Monad m => (a -> m b) -> ADT a -> m b

-- | Reduce an `ADT EnumeratedRoleType` with `f :: EnumeratedRoleType -> MP Boolean`
instance reducibleToBool :: Reducible EnumeratedRoleType Boolean where
  reduce f (ST a) = f a
  reduce f (SUM adts) = do
    (bools :: Array Boolean) <- traverse (reduce f) adts
    pure $ unwrap $ foldMap Conj bools
  reduce f (PROD adts) = do
    (bools :: Array Boolean) <- traverse (reduce f) adts
    pure $ unwrap $ foldMap Disj bools
  reduce f NOTYPE = pure false

-- | Reduce an `ADT EnumeratedRoleType` with `f :: EnumeratedRoleType -> MP (Array b)`
instance reducibleToArray :: Eq b => Reducible EnumeratedRoleType (Array b) where
  reduce f (ST a) = f a
  reduce f (SUM adts) = do
    (arrays :: Array (Array b)) <- traverse (reduce f) adts
    pure $ foldl intersect [] arrays
  reduce f (PROD adts) = do
    (arrays :: Array (Array b)) <- traverse (reduce f) adts
    pure $ foldl union [] arrays
  reduce f NOTYPE = pure []

-- | Reduce an `ADT EnumeratedRoleType` with `f :: EnumeratedRoleType -> MP (ADT EnumeratedRoleType)`
instance reducibletoADT :: Reducible EnumeratedRoleType (ADT EnumeratedRoleType) where
  reduce f (ST et) = f et
  reduce f (SUM adts) = do
    (x :: Array (ADT EnumeratedRoleType)) <- traverse (reduce f) adts
    -- Simplify: all members of the SUM must have a binding, otherwise the binding of the SUM is NOTYPE.
    case elemIndex NOTYPE x of
      Nothing -> pure NOTYPE
      otherwise -> pure $ SUM x
  reduce f (PROD adts) = do
    (x :: Array (ADT EnumeratedRoleType)) <- traverse (reduce f) adts
    -- Simplify: remove all NOTYPE's.
    pure $ PROD $ filter (notEq NOTYPE) x
  reduce f NOTYPE = pure NOTYPE
