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

module Perspectives.Representation.ExplicitSet where

import Prelude

import Data.Array (delete, elemIndex, find, foldM, foldl, intersect, nub, uncons)
import Data.Maybe (Maybe(..), isJust)
import Data.Set (subset, fromFoldable)
import Partial.Unsafe (unsafePartial)

-- | An ExplicitSet represents a set of elements, where we have an explicit representation of the
-- | Empty set and the Universal set.
data ExplicitSet a = Universal | Empty | PSet (Array a)

-- | Construct a PSet out of an array of PropertySets, handling Universal and Empty.
unionPset :: forall a. Eq a => Ord a => Array (ExplicitSet a) -> (ExplicitSet a)
unionPset terms = if isJust (elemIndex Universal terms)
  then Universal
  else PSet (unionOfArrays (unsafePartial elements <$> (delete Empty terms)))

unionOfArrays :: forall a. Ord a => Array (Array a) -> Array a
unionOfArrays = nub <<< join

intersectionOfArrays :: forall a. Eq a => Array (Array a) -> Array a
intersectionOfArrays x = case uncons x of
  Nothing -> []
  Just {head, tail} -> foldl intersect head tail

intersectionPset :: forall a. Eq a => Array (ExplicitSet a) -> ExplicitSet a
intersectionPset terms = if isJust (elemIndex Empty terms)
  then Empty
  else PSet (intersectionOfArrays (unsafePartial elements <$> (delete Universal terms)))

isElementOf :: forall a. Eq a => a -> ExplicitSet a -> Boolean
isElementOf a Empty = false
isElementOf a Universal = true
isElementOf a (PSet arr) = isJust (elemIndex a arr)

hasElement :: forall a. (a -> Boolean) -> ExplicitSet a -> Boolean
hasElement f Empty = false
hasElement f Universal = true
hasElement f (PSet arr) = isJust $ find f arr

hasElementM :: forall a m. Monad m => (a -> m Boolean) -> ExplicitSet a -> m Boolean
hasElementM f Empty = pure false
hasElementM f Universal = pure true
hasElementM f (PSet arr) = foldM g false arr
  where
    g found next =
      if found
        then pure found
        else f next

-- | p `subsetPSet` q is true iff all elements in p are in q.
subsetPSet :: forall a. Eq a => Ord a => ExplicitSet a -> ExplicitSet a -> Boolean
subsetPSet p q = case p, q of
    a, b | a == b -> true
    Empty, _ -> true
    _, Empty -> false
    Universal, _ -> false
    _, Universal -> true
    (PSet x), (PSet y) -> subset (fromFoldable x) (fromFoldable y)

elements :: forall a. Partial => ExplicitSet a -> Array a
elements (PSet s) = s

instance eqExplicitSet :: Eq a => Eq (ExplicitSet a) where
  eq (PSet s1) (PSet s2) = eq s1 s2
  eq Universal Universal = true
  eq Empty Empty = true
  eq _ _ = false

instance showExplicitSet :: Show a => Show (ExplicitSet a) where
  show Universal = "Universal"
  show Empty = "Empty"
  show (PSet s) = "PSet " <> show s
