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

module Perspectives.Representation.ADT2 where

import Data.Array (concat, elemIndex, findIndex, fold, intercalate, intersect, null, singleton, uncons, union)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.Set (fromFoldable, subset)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Partial.Unsafe (unsafePartial)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Applicative, class Eq, class Functor, class HeytingAlgebra, class Monoid, class Ord, class Show, bind, disj, flip, map, not, pure, show, ($), (/=), (<#>), (<$>), (<<<), (<>), (==), (&&))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

--------------------------------------------------------------------------------------------------
---- ADT
--------------------------------------------------------------------------------------------------

data ADT label a = 
  ST a 
  | UET a
  | CT label (ADT label a)
  | SUM (Array (ADT label a)) 
  | PROD (Array (ADT label a)) 
  
instance functorADT :: Functor (ADT label) where
  map f (ST a) = ST $ f a
  map f (UET a) = UET $ f a
  map f (CT label a) = CT label (map f a)
  map f (SUM adts) = SUM (map (map f) adts)
  map f (PROD adts) = PROD (map (map f) adts)

derive instance genericRepADT :: Generic (ADT label a) _

instance showADT :: (Show a, Show label) => Show (ADT label a) where
  show (ST a) = "(" <> "ST" <> " " <> show a <> ")"
  show (UET a) = "(" <> "UET" <> " " <> show a <> ")"
  show (CT label a) = "(" <> "CT " <> show label <> " " <> show a <> ")"
  show (SUM adts) = "(" <> "SUM" <> show adts <> ")"
  show (PROD adts) = "(" <> "PROD" <> show adts <> ")"

instance (Show a, Show label) => PrettyPrint (ADT label a) where 
  prettyPrint' t a@(ST _) = t <> show a
  prettyPrint' t a@(UET _) = t <> show a
  prettyPrint' t a@(CT _ _) = t <> show a
  prettyPrint' t (PROD terms) = t <> "PROD\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)
  prettyPrint' t (SUM terms) = t <> "SUM\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)

instance eqADT :: (Eq a, Eq label) => Eq (ADT label a) where
  eq b1 b2 = genericEq b1 b2

derive instance ordADT :: (Ord a, Ord label) => Ord (ADT label a)

--------------------------------------------------------------------------------------------------
---- FOLDABLE ADT, TRAVERSABLE ADT, AND FRIENDS
--------------------------------------------------------------------------------------------------

-- The foldmap function of this Foldable instance folds over SUM and PROD in the same way.
-- For a function that does justice to the notion of `sum` and `product`, see foldMapADT.
instance Foldable (ADT label) where
  foldMap f adt = case adt of 
    ST a -> f a
    UET a -> f a
    CT label a -> foldMap f a
    PROD as -> fold (foldMap f <$> as)
    SUM as -> fold (foldMap f <$> as)
  foldr a = foldrDefault a
  foldl a = foldlDefault a

instance Traversable (ADT label) where
  traverse f adt = case adt of
    ST a -> ST <$> f a
    UET a -> UET <$> f a
    CT label a -> CT label <$> traverse f a
    PROD as -> PROD <$> (traverse (traverse f) as)
    SUM as -> SUM <$> (traverse (traverse f) as)
  sequence a = sequenceDefault a

-- | This function is like Foldable foldMap, but it folds with Conj over PROD and with Disj over SUM.
foldMapADT :: forall a label h. HeytingAlgebra h => (a -> h) -> ADT label a -> h
foldMapADT f adt = case adt of
  ST a -> f a
  UET a -> f a
  CT label a -> foldMapADT f a
  PROD as -> unwrap $ fold (Conj <<< foldMapADT f <$> as)
  SUM as -> unwrap $ fold (Disj <<< foldMapADT f <$> as)

-- | This variant of foldMap folds with a function that is applied to the leaves in the tree,
-- | instead of to the values contained in them. Use it to collect labels along with values obtained 
-- | from a. A prime example is to collect all types on a fully expanded tree.
collect :: forall a label m. Monoid m => (ADT label a -> m) -> ADT label a -> m
collect f adt = case adt of 
    a@(ST _) -> f a
    a@(UET _) -> f a
    a@(CT _ _) -> f a
    PROD as -> fold (collect f <$> as)
    SUM as -> fold (collect f <$> as)

instance (WriteForeign a, WriteForeign label) => WriteForeign (ADT label a) where
  writeImpl f = writeImpl( genericSumToVariant f)

instance (ReadForeign a, ReadForeign label) => ReadForeign (ADT label a) where
  readImpl f = map variantToGenericSum (readImpl f)

--------------------------------------------------------------------------------------------------
---- EXPANDED ADT
--------------------------------------------------------------------------------------------------
data ExpandedADT label a = 
  EST a 
  | ECT label (ExpandedADT label a)
  | ESUM (Array (ExpandedADT label a)) 
  | EPROD (Array (ExpandedADT label a)) 

derive instance Generic (ExpandedADT label a) _

instance (Eq a, Eq label) => Eq (ExpandedADT label a) where
  eq b1 b2 = genericEq b1 b2

derive instance (Ord a, Ord label) => Ord (ExpandedADT label a)

instance (Show a, Show label) => Show (ExpandedADT label a) where
  show (EST a) = "(" <> "EST" <> " " <> show a <> ")"
  show (ECT label a) = "(" <> "ECT " <> show label <> " " <> show a <> ")"
  show (ESUM adts) = "(" <> "ESUM" <> show adts <> ")"
  show (EPROD adts) = "(" <> "EPROD" <> show adts <> ")"

instance (Show a, Show label) => PrettyPrint (ExpandedADT label a) where 
  prettyPrint' t a@(EST _) = t <> show a
  prettyPrint' t a@(ECT _ _) = t <> show a
  prettyPrint' t (EPROD terms) = t <> "EPROD\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)
  prettyPrint' t (ESUM terms) = t <> "ESUM\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)

-- The traverse function of the Traversable instance of ADT cannot change the structure of the
-- tree that describes a type. The function that traverses over the structure applies just to values 
-- contained in the structure.
-- `expand` is like traverse, but the function used to expand applies to the leaves of the tree, 
-- not just to the values contained in them. This makes it possible to 
--    * expand a leaf into a subtree (UET to CT);
--    * change one leaf type into another (UET to ST being the prime example)
expand :: forall a label m. Applicative m => (ADT label a -> m (ExpandedADT label a)) -> ADT label a -> m (ExpandedADT label a)
expand f adt = case adt of
  a@(ST _) -> f a
  a@(UET _) -> f a
  a@(CT _ _) -> f a
  PROD as -> EPROD <$> (traverse (expand f) as)
  SUM as -> ESUM <$> (traverse (expand f) as)

--------------------------------------------------------------------------------------------------
---- HARRAY: AN ARRAY THAT HAS AN INSTANCE OF HEYTINGALGEBRA
--------------------------------------------------------------------------------------------------
data HArray a = HArray (Array a) | Everything

fromHArray :: forall a. Partial => HArray a -> Array a
fromHArray (HArray a) = a

-- This instance allows us to create Conj and Disj instances of HArray - both monoids.
-- Those instances allow us to use foldMap to fold HAarray instances under either union or intersection.
-- We only need ff and tt to be able to implement mempty for the Monoid instances of Conj and Disj,
-- and conj and disj to implement append for the Semigroup instances of Conj and Disj.
instance Eq a => HeytingAlgebra (HArray a) where
  ff = HArray []
  tt = Everything
  conj l r = case l, r of
    (HArray a), (HArray b) -> HArray (intersect a b)
    a, Everything -> a
    Everything, b -> b
  disj l r = case l, r of
    (HArray a), (HArray b) -> HArray (union a b)
    _, _ -> Everything
  not a = case a of 
    Everything -> HArray []
    _ -> Everything
  implies a b = disj (not a) b

instance Show a => Show (HArray a) where
  show (HArray arr) = "HArray " <> show arr
  show Everything = "Everything"

-- | Compute a Boolean value by applying function f to the values contained in the leaves.
-- | The computation folds with logical AND over PROD and with logical OR over SUM.
computeBoolean :: forall label a. (a -> Boolean) -> ADT label a -> Boolean
computeBoolean f = foldMapADT f

-- | Compute a collection of values by applying function f to the values contained in the leaves.
-- | The computation folds with intersection over PROD and with union over SUM.
computeCollection :: forall label a b. Eq b => (a -> Array b) -> ADT label a -> Array b
computeCollection f = unsafePartial fromHArray <<< foldMapADT (HArray <<< f)

--------------------------------------------------------------------------------------------------
---- COMMONLEAVESINADT
--------------------------------------------------------------------------------------------------
-- | Every resulting value occurs in each branch from leaf to root.
-- | The computation folds with intersection over PROD and with union over SUM.
commonLeavesInADT :: forall label a. Eq a => ADT label a -> Array a
commonLeavesInADT = computeCollection singleton

--------------------------------------------------------------------------------------------------
---- ALLLEAVESINADT
--------------------------------------------------------------------------------------------------
-- | All values that occur in the tree's leaves.
-- | The computation folds with union over both PROD and SUM.
allLeavesInADT :: forall a label. Eq a => Ord a => ADT label a -> Array a
allLeavesInADT = foldMap singleton

--------------------------------------------------------------------------------------------------
---- DISJUNCTIVE NORMAL FORM
--------------------------------------------------------------------------------------------------
data DNF a = 
  DST a 
  | DSUM (Array (DNF a)) 
  | DPROD (Array (DNF a)) 

derive instance Generic (DNF a) _

instance (Eq a) => Eq (DNF a) where
  eq b1 b2 = genericEq b1 b2

derive instance (Ord a) => Ord (DNF a)

instance (Show a) => Show (DNF a) where
  show (DST a) = "(" <> "DST" <> " " <> show a <> ")"
  show (DSUM adts) = "(" <> "DSUM" <> show adts <> ")"
  show (DPROD adts) = "(" <> "DPROD" <> show adts <> ")"

instance (Show a) => PrettyPrint (DNF a) where 
  prettyPrint' t a@(DST _) = t <> show a
  prettyPrint' t (DPROD terms) = t <> "DPROD\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)
  prettyPrint' t (DSUM terms) = t <> "DSUM\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)

-- To be applied to an expanded tree only. In the expanded tree, UET will not occur.
toDisjunctiveNormalForm :: forall a label. Eq a => ExpandedADT label a -> DNF a
toDisjunctiveNormalForm adt = case adt of 
  EST a -> DSUM [(DPROD [DST a])]
  -- In the disjunctive normal form we have no UET.
  -- We have a tree built from EST, ECT, ESUM and EPROD.
  ECT label a -> toDisjunctiveNormalForm a
  ESUM as -> unsafePartial flattenSums $ map toDisjunctiveNormalForm as
  EPROD as -> unsafePartial distribute (map toDisjunctiveNormalForm as)

  where
    -- the argument is the product of applying toDisjunctiveNormalForm to an array of ADT - so it must
    -- be an array of SUM (PROD ST)
    flattenSums :: Partial => Array (DNF a) -> DNF a
    flattenSums sums = DSUM $ concat (sums <#>
      (\a -> case a of 
        DSUM ds -> ds))

    -- Because of context we can safely assume that adts is an Array of (SUM [(PROD [ST])])
    distribute :: Partial => Array (DNF a) -> DNF a
    distribute adts = DSUM $ (matrixMultiply $ adts <#> (\sum -> case sum of
      DSUM products -> products <#> (\product -> case product of 
        DPROD ps -> Conjunct ps))) <#> \(Conjunct terms) -> DPROD terms

    matrixMultiply :: forall b. Array (Array (Conjunct b)) -> Array (Conjunct b)
    matrixMultiply conjuncts = case uncons conjuncts of
      Nothing -> []
      Just {head, tail} -> if null tail
        then head
        else foldl multiply head tail
      where
      multiply :: Array (Conjunct b) -> Array (Conjunct b) -> Array (Conjunct b)
      multiply cjs1 cjs2 = do
        Conjunct cj1 <- cjs1
        Conjunct cj2 <- cjs2
        pure $ Conjunct $ cj1 <> cj2

newtype Conjunct a = Conjunct (Array a)

--------------------------------------------------------------------------------------------------
---- SPECIALISESADT
--------------------------------------------------------------------------------------------------
-- | left `equalsOrSpecialisesADT` right
-- | left <= right
-- | See equalsOrSpecialisesADT_.
-- | See: Semantics of the Perspectives Language, chapter Another ordering of Role types for an explanation.
equalsOrSpecialisesADT :: forall a label. Ord a => Ord label => Eq a => Ord label => ExpandedADT label a -> ExpandedADT label a -> Boolean
equalsOrSpecialisesADT adt1 adt2 = let
  (adt1' :: DNF a) = toDisjunctiveNormalForm adt1
  adt2' = toDisjunctiveNormalForm adt2
  in
  equalsOrSpecialisesADT_ adt1' adt2'

-- | left `equalsOrGeneralisesADT` right
-- | left >= right
equalsOrGeneralisesADT :: forall a label. Ord a => Ord label => Eq a => Ord label => ExpandedADT label a -> ExpandedADT label a -> Boolean
equalsOrGeneralisesADT = flip equalsOrSpecialisesADT

-- | left `equalsOrSpecialisesADT_` right
-- | left <= right
-- | Intuitively when left is built by specialising right.
-- | Or, of course, when both are equal.
-- | Construct left as a specialisation of right by:
-- |    - adding a term to it (when right is a product)
-- |    - removing a term from it (when right is a sum).
equalsOrSpecialisesADT_ :: forall a. Ord a => Eq a => DNF a -> DNF a -> Boolean
equalsOrSpecialisesADT_ leftside rightside = 
  case leftside of 
    left@(DST _) -> case rightside of
      right@(DST _) -> left == right
      DPROD rights -> [left] == rights
      DSUM rights -> isJust $ findIndex (\right -> left `equalsOrSpecialisesADT_` right) rights
    DPROD lefts -> case rightside of
      right@(DST _) -> isJust $ elemIndex right lefts
      DPROD rights -> lefts `superset` rights
      DSUM rights -> isJust $ findIndex (\right -> DPROD lefts `equalsOrSpecialisesADT_` right) rights 
    DSUM lefts -> foldl (\allTrue left -> if allTrue then left `equalsOrSpecialisesADT_` rightside else false) true lefts
  where
  superset :: forall x. Ord x => Eq x => Array x -> Array x -> Boolean
  superset super sub = (fromFoldable sub) `subset` (fromFoldable super)

-- | left `generalisesADT` right
-- | left > right
generalisesADT :: forall a label. Ord a => Ord label => Eq a => Ord label => ExpandedADT label a -> ExpandedADT label a -> Boolean
generalisesADT adt1 adt2 = adt1 /= adt2 && adt1 `equalsOrGeneralisesADT` adt2

-- | left `specialisesADT` right
-- | left < right
specialisesADT :: forall a label. Ord a => Ord label => Eq a => Ord label => ExpandedADT label a -> ExpandedADT label a -> Boolean
specialisesADT = flip generalisesADT
