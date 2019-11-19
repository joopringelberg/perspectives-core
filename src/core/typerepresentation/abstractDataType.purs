-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

-- | An Algebraic Data Type to represent the type of roles with more than one type of binding.
-- | `a` invariably is a newtype representing entities on the type level of Perspectives.
-- | The dataconstructor ST is by construction used just with `EnumeratedRoleType`, never with `CalculatedRoleType`.
-- |
-- | ADT's are constructed for ContextType, too.
-- |
-- | ### Equivalences
-- | * `SUM x NOTYPE` equals `x`
-- |
-- | * `PROD x NOTYPE` equals `NOTYPE`

module Perspectives.Representation.ADT where

import Data.Array (elemIndex, filter, head, intersect, uncons, union)
import Data.Foldable (foldMap, foldl)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Foreign (unsafeFromForeign, unsafeToForeign)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType)
import Prelude (class Eq, class Monad, class Show, bind, flip, map, notEq, pure, ($), (==), (<>), show)
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON', writeJSON)

data ADT a = ST a | SUM (Array (ADT a)) | PROD (Array (ADT a)) | NOTYPE

derive instance genericRepBinding :: Generic (ADT a) _

instance showADT :: (Show a) => Show (ADT a) where
  show (ST a) = "(" <> "ST" <> " " <> show a <> ")"
  show NOTYPE = "NOTYPE"
  show (SUM adts) = "(" <> "SUM" <> show adts <> ")"
  show (PROD adts) = "(" <> "PROD" <> show adts <> ")"

instance eqADT :: (Eq a) => Eq (ADT a) where
  eq b1 b2 = genericEq b1 b2

instance writeForeignADT :: (WriteForeign a) => WriteForeign (ADT a) where
  writeImpl b = unsafeToForeign (writeJSON b)

instance readForeignADT :: (ReadForeign a) => ReadForeign (ADT a) where
  readImpl b = readJSON' (unsafeFromForeign b)

-- | The `Reducible` class implements a pattern to recursively process an ADT.
class Reducible a b where
  reduce :: forall m. Monad m => (a -> m b) -> ADT a -> m b

-- | Reduce an `ADT EnumeratedRoleType` with `f :: EnumeratedRoleType -> MP Boolean`
-- | Does **not** take the binding of a role into account.
instance reducibleToBool :: Reducible EnumeratedRoleType Boolean where
  reduce f (ST a) = f a
  reduce f (SUM adts) = do
    (bools :: Array Boolean) <- traverse (reduce f) adts
    pure $ unwrap $ foldMap Conj bools
  reduce f (PROD adts) = do
    (bools :: Array Boolean) <- traverse (reduce f) adts
    pure $ unwrap $ foldMap Disj bools
  reduce f NOTYPE = pure false

-- | Reduce an `ADT a` with `f :: a -> MP (Array b)`
-- | Includes the binding of a role: this means that the computation recurses on the binding.
-- | This is expected behaviour for functions like `propertiesOfADT`, `viewsOfADT` and `roleAspectsOfADT`.
instance reducibleToArray :: Eq b => Reducible a (Array b) where
  reduce f (ST a) = f a
  reduce f (SUM adts) = do
    (arrays :: Array (Array b)) <- traverse (reduce f) adts
    pure $ foldl intersect [] arrays
  reduce f (PROD adts) = do
    (arrays :: Array (Array b)) <- traverse (reduce f) adts
    pure $ foldl union [] arrays
  reduce f NOTYPE = pure []

-- | Reduce an `ADT a` with `f :: a -> MP (ADT b)`.
-- | `reduce f` then has type `ADT a` -> MP (ADT b)`.
instance reducibletoADT :: Eq b => Reducible a (ADT b) where
  reduce f (ST et) = f et
  reduce f (SUM adts) = do
    (x :: Array (ADT b)) <- traverse (reduce f) adts
    -- Simplify: all members of the SUM must have a binding, otherwise the binding of the SUM is NOTYPE.
    case elemIndex NOTYPE x of
      Nothing -> pure NOTYPE
      otherwise -> pure $ SUM x
  reduce f (PROD adts) = do
    (x :: Array (ADT b)) <- traverse (reduce f) adts
    -- Simplify: remove all NOTYPE's.
    r <- pure $ filter (notEq NOTYPE) x
    case uncons r of
      -- SUM [] == NOTYPE
      Nothing -> pure NOTYPE
      (Just {head : hd, tail}) -> case head tail of
        -- SUM [a] = a
        Nothing -> pure hd
        otherwise -> pure $ PROD r
  reduce f NOTYPE = pure NOTYPE

-- | `q greaterThanOrEqualTo p` means: q is more specific than p, or equal to p
-- | If you use `less specific` instead of `more specific`, flip the arguments.
-- | If you use `more general` instead of `more specific`, flip them, too.
-- | So `less specific` instead of `more general` means flipping twice and is a no-op.
-- | Therefore `less specific` equals `more general`.
greaterThanOrEqualTo :: forall a. Eq a => ADT a -> ADT a -> Boolean
greaterThanOrEqualTo = flip lessThanOrEqualTo

-- | `p lessThanOrEqualTo q` means: p is less specific than q, or equal to q.
-- | `p lessThanOrEqualTo q` equals: `q greaterThanOrEqualTo p`
-- | This function is semantically correct only on a fully expanded type: use `Perspectives.Representation.Class.Role.expandedADT`.
lessThanOrEqualTo :: forall a. Eq a => ADT a -> ADT a -> Boolean
lessThanOrEqualTo (ST x) (ST y) = x == y
-- p must be less than or equal to every member of q.
lessThanOrEqualTo p@(ST _) (SUM adts) = let
  (bools :: Array Boolean) = map (lessThanOrEqualTo p) adts
  in unwrap $ foldMap Conj bools
-- p must be less than or equal to one of the members of q.
lessThanOrEqualTo p@(ST _) (PROD adts) = let
  (bools :: Array Boolean) = map (lessThanOrEqualTo p) adts
  in unwrap $ foldMap Disj bools
-- NOTYPE is less specific than any other type.
lessThanOrEqualTo (ST _) NOTYPE = false
-- A sum is less than or equal to a type if that type is greater than or equal to any of its members.
lessThanOrEqualTo (SUM adts) q = let
  (bools :: Array Boolean) = map (greaterThanOrEqualTo q) adts
  in unwrap $ foldMap Conj bools
-- A product is less than or equal to a type if that type is greater than or equal to one of its members.
lessThanOrEqualTo (PROD adts) q = let
  (bools :: Array Boolean) = map (greaterThanOrEqualTo q) adts
  in unwrap $ foldMap Disj bools

-- p = NOTYPE
-- q = _
-- lessThanOrEqualTo _ NOTYPE = false
-- (q <= p) = false
-- not (q <= p) = true
-- not (q <= p) ==> q > p ==> p <= q
lessThanOrEqualTo NOTYPE _ = true
