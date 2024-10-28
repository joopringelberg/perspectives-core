-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
-- SPDX-License-Identifier: GPL-3.0-or-later
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
-- Full text of this license can be found in the LICENSE directory in the projects root.

-- END LICENSE

module Perspectives.Representation.ExplicitSet where
 
import Prelude

import Data.Array (delete, elemIndex, find, foldM, foldl, intersect, nub, null, uncons)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Set (subset, fromFoldable)
import Partial.Unsafe (unsafePartial)
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

-----------------------------------------------------------
-- EXPLICITSET
-----------------------------------------------------------
-- | An ExplicitSet represents a set of elements, where we have an explicit representation of the
-- | Empty set and the Universal set.
data ExplicitSet a = Universal | Empty | PSet (Array a)

derive instance genericExplicitSet :: Generic (ExplicitSet a) _
instance eqExplicitSet :: Eq a => Eq (ExplicitSet a) where
  eq (PSet s1) (PSet s2) = eq s1 s2
  eq Universal Universal = true
  eq Empty Empty = true
  eq _ _ = false
instance showExplicitSet :: Show a => Show (ExplicitSet a) where
  show Universal = "Universal"
  show Empty = "Empty"
  show (PSet s) = "PSet " <> show s
instance writeForeignExplicitSet :: WriteForeign a => WriteForeign (ExplicitSet a) where
  writeImpl Universal = writeImpl {constructor: "Universal", set: ([] :: Array a)}
  writeImpl Empty = writeImpl {constructor: "Empty", set: ([] :: Array a)}
  writeImpl (PSet s) = writeImpl {constructor: "PSet", set: s}
instance ReadForeign a => ReadForeign (ExplicitSet a) where
  readImpl f = do 
    {constructor, set} :: {constructor :: String, set :: Array a} <- read' f
    unsafePartial case constructor of 
      "Universal" -> pure Universal
      "Empty" -> pure Empty
      "PSet" -> pure $ PSet set

instance functorExplicitSet :: Functor ExplicitSet where
  map f Universal = Universal
  map f Empty = Empty
  map f (PSet as) = PSet (f <$> as)

instance semigroupExplicitSet :: Semigroup (ExplicitSet a) where
  append Universal _ = Universal
  append _ Universal = Universal
  append Empty x = x
  append x Empty = x
  append (PSet a1) (PSet a2) = PSet (a1 <> a2)

-----------------------------------------------------------
-- FUNCTIONS ON EXPLICITSET
-----------------------------------------------------------
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
  else let 
      i = (intersectionOfArrays (unsafePartial elements <$> (delete Universal terms)))
    in
      if null i 
        then Empty
        else PSet i  

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

overlapsPSet :: forall a. Eq a => Ord a => ExplicitSet a -> ExplicitSet a -> Boolean
overlapsPSet p q = case p, q of
    a, b | a == b -> true
    Empty, _ -> false
    _, Empty -> false
    Universal, _ -> true
    _, Universal -> true
    (PSet x), (PSet y) -> foldl
      (\result nextY -> if result
        then result
        else isJust $ elemIndex nextY x)
      false
      y

elements :: forall a. Partial => ExplicitSet a -> Array a
elements (PSet s) = s
