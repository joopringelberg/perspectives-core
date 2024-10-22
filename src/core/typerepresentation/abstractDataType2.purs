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

-- | An Algebraic Data Type to represent the type of roles with more than one type of binding.
-- | `a` invariably is a newtype representing entities on the type level of Perspectives.
-- | The dataconstructor ST is by construction used just with `EnumeratedRoleType`, never with `CalculatedRoleType`.
-- |
-- | ADT's are constructed for ContextType, too.
-- |
-- | ### Equivalences
-- | * `SUM x EMPTY` equals `x`
-- |
-- | * `PROD x EMPTY` equals `EMPTY`

module Perspectives.Representation.ADT where

import Data.Array (catMaybes, findIndex, fold, intercalate, intersect, length, nub, singleton, union)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldMap, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.Set (fromFoldable, subset) as SET
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.CNF (CNF, DPROD(..), DSUM(..), toConjunctiveNormalForm)
import Perspectives.Representation.ExpandedADT (ExpandedADT(..), foldMapExpandedADT)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Applicative, class Bind, class Eq, class Functor, class HeytingAlgebra, class Monoid, class Ord, class Show, disj, flip, map, not, pure, show, ($), (&&), (/=), (<#>), (<$>), (<<<), (<>), (>>=), (==), bind)
import Simple.JSON (class ReadForeign, class WriteForeign, writeImpl, writeJSON, read', readJSON')

--------------------------------------------------------------------------------------------------
---- ADT
--------------------------------------------------------------------------------------------------

data ADT a = 
  ST a 
  | UET a
  | SUM (Array (ADT a)) 
  | PROD (Array (ADT a)) 
  
instance functorADT :: Functor ADT where
  map f (ST a) = ST $ f a
  map f (UET a) = UET $ f a
  map f (SUM adts) = SUM (map (map f) adts)
  map f (PROD adts) = PROD (map (map f) adts)

derive instance genericRepADT :: Generic (ADT a) _

instance showADT :: (Show a) => Show (ADT a) where
  show (ST a) = "(" <> "ST" <> " " <> show a <> ")"
  show (UET a) = "(" <> "UET" <> " " <> show a <> ")"
  show (SUM adts) = "(" <> "SUM" <> show adts <> ")"
  show (PROD adts) = "(" <> "PROD" <> show adts <> ")"

instance (Show a) => PrettyPrint (ADT a) where 
  prettyPrint' t a@(ST _) = t <> show a
  prettyPrint' t a@(UET _) = t <> show a
  prettyPrint' t (PROD terms) = t <> "(PROD [\n" <> (intercalate ",\n" $ prettyPrint' (t <> "  ") <$> terms) <> "\n" <> t <> "  ])"
  prettyPrint' t (SUM terms) = t <> "(SUM [\n" <> (intercalate ",\n" $ prettyPrint' (t <> "  ") <$> terms) <> "\n" <> t <> "  ])"

instance eqADT :: (Eq a) => Eq (ADT a) where
  eq b1 b2 = genericEq b1 b2

derive instance ordADT :: (Ord a) => Ord (ADT a)

--------------------------------------------------------------------------------------------------
---- FOLDABLE ADT, TRAVERSABLE ADT, AND FRIENDS
--------------------------------------------------------------------------------------------------

-- The foldmap function of this Foldable instance folds over SUM and PROD in the same way.
-- For a function that does justice to the notion of `sum` and `product`, see foldMapADT.
instance Foldable ADT where
  foldMap f adt = case adt of 
    ST a -> f a
    UET a -> f a
    PROD as -> fold (foldMap f <$> as)
    SUM as -> fold (foldMap f <$> as)
  foldr a = foldrDefault a
  foldl a = foldlDefault a

instance Traversable ADT where
  traverse f adt = case adt of
    ST a -> ST <$> f a
    UET a -> UET <$> f a
    PROD as -> PROD <$> (traverse (traverse f) as)
    SUM as -> SUM <$> (traverse (traverse f) as)
  sequence a = sequenceDefault a

-- | This function is like Foldable foldMap, but it folds with Conj over PROD and with Disj over SUM.
foldMapADT :: forall a h. HeytingAlgebra h => (a -> h) -> ADT a -> h
foldMapADT f adt = case adt of
  ST a -> f a
  UET a -> f a
  PROD as -> unwrap $ fold (Conj <<< foldMapADT f <$> as)
  SUM as -> unwrap $ fold (Disj <<< foldMapADT f <$> as)

-- | This variant of foldMap folds with a function that is applied to the leaves in the tree,
-- | instead of to the values contained in them. Use it to collect labels along with values obtained 
-- | from a. A prime example is to collect all types on a fully expanded tree.
collect :: forall a m. Monoid m => (ADT a -> m) -> ADT a -> m
collect f adt = case adt of 
    a@(ST _) -> f a
    a@(UET _) -> f a
    PROD as -> fold (collect f <$> as)
    SUM as -> fold (collect f <$> as)

instance (WriteForeign a) => WriteForeign (ADT a) where
  writeImpl (ST a) = writeImpl {constructor: "ST", arg: writeJSON a}
  writeImpl (UET a) = writeImpl {constructor: "UET", arg: writeJSON a}
  writeImpl (SUM as) = writeImpl { constructor: "SUM", arg: writeJSON as}
  writeImpl (PROD as) = writeImpl { constructor: "PROD", arg: writeJSON as}


instance (ReadForeign a) => ReadForeign (ADT a) where
  readImpl f = do
    x :: {constructor :: String, arg :: String} <- read' f
    unsafePartial case x.constructor, x.arg of
      "ST", a -> ST <$> readJSON' a
      "UET", a -> UET <$> readJSON' a
      "SUM", as -> SUM <$> readJSON' as
      "PROD", as -> PROD <$> readJSON' as

--------------------------------------------------------------------------------------------------
---- HARRAY: AN ARRAY THAT HAS AN INSTANCE OF HEYTINGALGEBRA
--------------------------------------------------------------------------------------------------
data HArray a = HArray (Array a) | Everything

fromHArray :: forall a. Partial => HArray a -> Array a
fromHArray (HArray a) = a

-- This instance allows us to create Conj and Disj instances of HArray - both monoids.
-- Those instances allow us to use foldMap to fold HAarray instances under either union or intersection.
-- We only need tt and ff to be able to implement mempty for the Monoid instances of Conj and Disj respectively,
-- and conj and disj to implement append for the Semigroup instances of Conj and Disj.
-- An HArray constructed with conj or disj will not have duplicates.
instance (Ord a, Eq a) => HeytingAlgebra (HArray a) where
  -- ff is used to implement mempty for the Disj Monoid.
  ff = Everything
  -- It would be reasonable to have tt be Everything.
  -- However, we cannot handle that in practice. We never want Everything in our computation.
  -- tt is used to implement mempty for the Conj Monoid. 
  tt = HArray []
  -- tt = HArray []
  conj l r = case l, r of
    (HArray a), (HArray b) -> HArray (union (nub a) b)
    (HArray a), Everything -> Everything
    Everything, (HArray b) -> Everything
    Everything, Everything -> Everything
  disj l r = case l, r of
    (HArray a), (HArray b) -> HArray (intersect (nub a) b)
    (HArray a), Everything -> HArray (nub a)
    Everything, (HArray b) -> HArray (nub b)
    Everything, Everything -> Everything
  not a = case a of 
    Everything -> HArray []
    _ -> Everything
  implies a b = disj (not a) b

instance Show a => Show (HArray a) where
  show (HArray arr) = "HArray " <> show arr
  show Everything = "Everything"

-- | Compute a Boolean value by applying function f to the values contained in the leaves.
-- | The computation folds with logical AND over PROD and with logical OR over SUM.
computeBoolean :: forall a. (a -> Boolean) -> ADT a -> Boolean
computeBoolean f = foldMapADT f

-- | Compute a Boolean value by applying function f to the values contained in the leaves.
-- | The computation folds with logical AND over PROD and with logical OR over SUM.
computeExpandedBoolean :: forall a. (a -> Boolean) -> ExpandedADT a -> Boolean
computeExpandedBoolean f = foldMapExpandedADT f

-- | Compute a collection of values by applying function f to the values contained in the leaves.
-- | The computation folds with intersection over PROD and with union over SUM.
computeCollection :: forall a b. Ord b => Eq b => (a -> Array b) -> ADT a -> Array b
computeCollection f = unsafePartial fromHArray <<< foldMapADT (HArray <<< f)

-- | Compute a collection of values by applying function f to the values contained in the leaves.
-- | The computation folds with intersection over PROD and with union over SUM.
computeExpandedCollection :: forall a b. Ord b => Eq b => (a -> Array b) -> ExpandedADT a -> Array b
computeExpandedCollection f = unsafePartial fromHArray <<< foldMapExpandedADT (HArray <<< f)

--------------------------------------------------------------------------------------------------
---- COMMONLEAVESINADT
--------------------------------------------------------------------------------------------------
-- | Every resulting value occurs in each branch from leaf to root.
-- | The computation folds with intersection over PROD and with union over SUM.
commonLeavesInADT :: forall a. Ord a => Eq a => ADT a -> Array a
commonLeavesInADT = computeCollection singleton

--------------------------------------------------------------------------------------------------
---- ALLLEAVESINADT
--------------------------------------------------------------------------------------------------
-- | All values that occur in the tree's leaves.
-- | The computation folds with union over both PROD and SUM.
allLeavesInADT :: forall a. Eq a => Ord a => ADT a -> Array a
allLeavesInADT = foldMap singleton

allLeavesInExpandedADT :: forall a. Eq a => Ord a => ExpandedADT a -> Array a
allLeavesInExpandedADT = foldMap singleton

--------------------------------------------------------------------------------------------------
---- EXPANDING ADT
--------------------------------------------------------------------------------------------------
-- The traverse function of the Traversable instance of ADT cannot change the structure of the
-- tree that describes a type. The function that traverses over the structure applies just to values 
-- contained in the structure.
-- `expand` is like traverse, but the function used to expand applies to the leaves of the tree, 
-- not just to the values contained in them. This makes it possible to 
--    * expand a leaf into a subtree (UET to CT);
--    * change one leaf type into another (UET to ST being the prime example)
expand :: forall a m. Applicative m => Ord a => (ADT a -> m (ExpandedADT a)) -> ADT a -> m (ExpandedADT a)
expand f adt = case adt of
  a@(ST _) -> f a
  a@(UET _) -> f a
  PROD as -> EPROD <<< nub <$> (traverse (expand f) as)
  SUM as -> ESUM <<< nub <$> (traverse (expand f) as)

-- | Like expand, but returns an ADT.
transform :: forall a m. Applicative m => (ADT a -> m (ADT a)) -> ADT a -> m (ADT a)
transform f adt = case adt of
  a@(ST _) -> f a
  a@(UET _) -> f a
  PROD as -> PROD <$> (traverse (transform f) as)
  SUM as -> SUM <$> (traverse (transform f) as)

-- | Like expand, but with the added possibility of reducing a node to Nothing.
-- | NOTE: the result is not an ExpandedADT; just ADT.
expandAndReduce :: forall a m. Bind m => Applicative m => (ADT a -> m (Maybe (ADT a))) -> ADT a -> m (Maybe (ADT a))
expandAndReduce f adt = case adt of
  a@(ST _) -> f a
  a@(UET _) -> f a
  -- PROD as -> Just <<< PROD <<< catMaybes <$> (traverse (expandAndReduce f) as)
  PROD as -> traverse (expandAndReduce f) as >>= \r -> case catMaybes r of 
    [] -> pure Nothing
    terms -> pure $ Just $ PROD terms
  SUM as -> (traverse (expandAndReduce f) as) >>= \r -> case catMaybes r of
    [] -> pure Nothing
    terms -> pure $ Just $ SUM terms

--------------------------------------------------------------------------------------------------
---- COMPARING ABSTRACT DATA TYPES
-- | We compare types by considering them to be propositional formulae. 
-- | The notion of implication takes central place. A formula implies another iff all of its models also satisfy the other formula.
-- | We compute this only on ExpandedADT (when no more type substitutions can be made) and then only over its distributed normal form.
-- | This facilitates computing: one product of terms implies another iff it is a (not necessarily strict) superset of the other.
-- | One product implies a sum of products iff it implies of of its terms;
-- | finally, one sum implies another if all of its product terms imply the other.
-- | NOTICE how this compares to foldMapExpandedADT and foldMapADT. There we treat PROD as a Conj(tive) 
-- | data structure and when that structure is an HArray, we compute that conjunction by intersection (and SUM by union).
--------------------------------------------------------------------------------------------------
-- | left `equalsOrSpecialises` right
-- | left -> right
-- | See equalsOrSpecialises_.
-- | See: Semantics of the Perspectives Language, chapter Another ordering of Role types for an explanation.
equalsOrSpecialises :: forall a. Ord a => Eq a => ExpandedADT a -> ExpandedADT a -> Boolean
equalsOrSpecialises adt1 adt2 = let
  (adt1' :: CNF a) = toConjunctiveNormalForm adt1
  adt2' = toConjunctiveNormalForm adt2
  in
  equalsOrSpecialises_ adt1' adt2'

-- | left `equalsOrSpecialises_` right
-- | From the technical documentation:
-- | * under `R specialises X` we take to understand `R => X`
-- | * R => X iff
-- | * For every disjunction x in the conjunctive formula of X, 
-- | * there is a disjunction r in the conjunctive formula of R for which is it true that the terms of r are a subset of the terms of x.
-- |
-- | In terms of left and right (substitute left for R and right for X):
-- | * For every disjunction r in the conjunctive formula of right, 
-- | * there is a disjunction l in the conjunctive formula of left for which is it true that the terms of l are a subset of the terms of r.
equalsOrSpecialises_ :: forall a. Ord a => Eq a => CNF a -> CNF a -> Boolean
equalsOrSpecialises_ = unsafePartial equalsOrSpecialises'
  where
    equalsOrSpecialises' :: CNF a -> CNF a -> Boolean
    equalsOrSpecialises' (DPROD disjunctionsOnLeft) (DPROD disjunctionsOnRight) = unwrap
      (fold
        (disjunctionsOnRight <#> \(DSUM disjunctionOnRight) -> Conj $ isJust $ (flip findIndex) disjunctionsOnLeft
          \(DSUM disjunctionOnLeft) -> ((SET.fromFoldable disjunctionOnLeft) `SET.subset` (SET.fromFoldable disjunctionOnRight))))

equals_ :: forall a. Ord a => Eq a => CNF a -> CNF a -> Boolean
equals_ = unsafePartial equals'
  where
    equals' :: CNF a -> CNF a -> Boolean
    equals' (DPROD disjunctionsOnLeft) (DPROD disjunctionsOnRight) = length disjunctionsOnLeft == length disjunctionsOnRight && unwrap
      (fold
        (disjunctionsOnRight <#> \(DSUM disjunctionOnRight) -> Conj $ isJust $ (flip findIndex) disjunctionsOnLeft
          \(DSUM disjunctionOnLeft) -> ((SET.fromFoldable disjunctionOnLeft) == (SET.fromFoldable disjunctionOnRight))))

-- | left `specialises` right
-- | left -> right
specialises :: forall a. Ord a => Eq a => ExpandedADT a -> ExpandedADT a -> Boolean
specialises = flip generalises

specialises_ :: forall a. Ord a => Eq a => CNF a -> CNF a -> Boolean
specialises_ = flip generalises_

-- | left `equalsOrGeneralises` right
-- | left <- right (NOTICE REVERSED ARROW)
equalsOrGeneralises :: forall a. Ord a => Eq a => ExpandedADT a -> ExpandedADT a -> Boolean
equalsOrGeneralises = flip equalsOrSpecialises

equalsOrGeneralises_ :: forall a. Ord a => Eq a => CNF a -> CNF a -> Boolean
equalsOrGeneralises_ = flip equalsOrSpecialises_

-- | left `generalises` right
-- | left <- right (NOTICE REVERSED ARROW)
generalises :: forall a. Ord a => Eq a => ExpandedADT a -> ExpandedADT a -> Boolean
generalises adt1 adt2 = adt1 /= adt2 && adt1 `equalsOrGeneralises` adt2

generalises_ :: forall a. Ord a => Eq a => CNF a -> CNF a -> Boolean
generalises_ dnf1 dnf2 = dnf1 /= dnf2 && dnf1 `equalsOrGeneralises_` dnf2