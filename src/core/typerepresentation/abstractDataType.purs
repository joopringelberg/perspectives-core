module Perspectives.Representation.ADT where

import Data.Array (intersect, union)
import Data.Foldable (foldMap, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType)
import Prelude (class Eq, class Monad, class Show, ($), bind, pure)
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

class Reducable a b where
  reduce :: forall m. Monad m => (a -> m b) -> ADT a -> m b

instance reducableToBool :: Reducable EnumeratedRoleType Boolean where
  reduce f (ST a) = f a
  reduce f (SUM adts) = do
    (bools :: Array Boolean) <- traverse (reduce f) adts
    pure $ unwrap $ foldMap Conj bools
  reduce f (PROD adts) = do
    (bools :: Array Boolean) <- traverse (reduce f) adts
    pure $ unwrap $ foldMap Disj bools
  reduce f NOTYPE = pure false

instance reducableToArray :: Eq b => Reducable EnumeratedRoleType (Array b) where
  reduce f (ST a) = f a
  reduce f (SUM adts) = do
    (arrays :: Array (Array b)) <- traverse (reduce f) adts
    pure $ foldl intersect [] arrays
  reduce f (PROD adts) = do
    (arrays :: Array (Array b)) <- traverse (reduce f) adts
    pure $ foldl union [] arrays
  reduce f NOTYPE = pure []
