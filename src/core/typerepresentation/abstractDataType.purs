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

module Perspectives.Representation.ADT.Old where

import Data.Array (catMaybes, concat, elemIndex, filter, findIndex, intercalate, intersect, length, nub, null, singleton, uncons, union)
import Data.Array.Partial (head) as AP
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldMap, foldl)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (fromFoldable, subset)
import Data.Traversable (traverse)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Partial.Unsafe (unsafePartial)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, class Ord, class Show, bind, eq, flip, map, pure, show, ($), (&&), (/=), (<$>), (<*>), (<<<), (<>), (==), (>>>))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

data ADT a = ST a | EMPTY | SUM (Array (ADT a)) | PROD (Array (ADT a)) | UNIVERSAL

instance functorADT :: Functor ADT where
  map f (ST a) = ST $ f a
  map f EMPTY = EMPTY
  map f (SUM adts) = SUM (map (map f) adts)
  map f (PROD adts) = PROD (map (map f) adts)
  map f UNIVERSAL = UNIVERSAL

derive instance genericRepADT :: Generic (ADT a) _

instance showADT :: (Show a) => Show (ADT a) where
  show (ST a) = "(" <> "ST" <> " " <> show a <> ")"
  show EMPTY = "EMPTY"
  show (SUM adts) = "(" <> "SUM" <> show adts <> ")"
  show (PROD adts) = "(" <> "PROD" <> show adts <> ")"
  show UNIVERSAL = "UNIVERSAL"

instance x :: (Show a) => PrettyPrint (ADT a) where 
  prettyPrint' t a@(ST _) = t <> show a
  prettyPrint' t (PROD terms) = t <> "PROD\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)
  prettyPrint' t (SUM terms) = t <> "SUM\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)
  prettyPrint' t UNIVERSAL = t <> "UNIVERSAL"
  prettyPrint' t EMPTY = t <> "EMPTY"
  

instance eqADT :: (Eq a) => Eq (ADT a) where
  eq b1 b2 = genericEq b1 b2

instance writeForeignADT :: WriteForeign a => WriteForeign (ADT a) where
  writeImpl f = writeImpl( genericSumToVariant f)

instance readForeightADT :: ReadForeign a => ReadForeign (ADT a) where
  readImpl f = map variantToGenericSum (readImpl f)

derive instance ordADT :: Ord a => Ord (ADT a)

-- | SUM with simplifications:
-- | SUM [EMPTY, ..] = EMPTY
-- | SUM [UNIVERSAL, ..] = [..]
-- | SUM [SUM [..], x] = SUM [x, ..]
-- | SUM [SUM [..], SUM[..]] = SUM [.. ..]
-- | SUM [x, .. x] = SUM[.., x]
-- | SUM [x] = x
sum :: forall a. Eq a => Array (ADT a) -> ADT a
sum = foldl sum' UNIVERSAL >>> \(s :: ADT a) -> case s of
    SUM x | length x == 1 -> unsafePartial $ AP.head x
    otherwise -> otherwise
  where
  sum' :: ADT a -> ADT a -> ADT a
  sum' EMPTY _ = EMPTY
  sum' UNIVERSAL x = x
  sum' (SUM terms1) (SUM terms2) = SUM (union terms1 terms2)
  sum' (SUM terms) x = SUM (union terms [x])
  sum' t1@(ST x) t2@(ST y) = if x == y then t1 else SUM [t1, t2]
  sum' a b = SUM [a, b]

-- | PRODUCT with simplifications:
-- | PRODUCT [UNIVERSAL, ..] = UNIVERSAL
-- | PRODUCT [EMPTY, ..] = [..]
-- | PRODUCT [PRODUCT[..], ST x] = PRODUCT[.., x]
-- | PRODUCT [PRODUCT[..], PRODUCT[..]] = PRODUCT[.. ..]
-- | PRODUCT [x, .. x] = PRODUCT[.., x]
-- | PRODUCT [x] = x
product :: forall a. Eq a => Array (ADT a) -> ADT a
product = (foldl prod' EMPTY) >>> \(p :: ADT a) -> case p of
    PROD x | length x == 1 -> unsafePartial $ AP.head x
    otherwise -> otherwise
  where
  prod' :: ADT a -> ADT a -> ADT a
  prod' UNIVERSAL _ = UNIVERSAL
  prod' EMPTY x = x
  prod' (PROD terms1) (PROD terms2) = PROD (union terms1 terms2)
  prod' (PROD terms) x = PROD (union terms [x])
  prod' t1@(ST x) t2@(ST y) = if x == y then t1 else PROD [t1, t2]
  prod' a b = PROD [a, b]

-- | Sums and products in binary notation.
data ADT' a = ST' a | SUM' (ADT' a) (ADT' a) | PROD' (ADT' a) (ADT' a) | EMPTY' | UNIVERSAL'

derive instance Generic (ADT' a) _
instance (Eq a) => Eq (ADT' a) where
  eq b1 b2 = genericEq b1 b2

newtype Disjunct a = Disjunct (Array (Conjunct a)) 
newtype Conjunct a = Conjunct (Array a)

toDisjunctiveNormalForm :: forall a. Eq a => ADT a -> ADT a
toDisjunctiveNormalForm adt = fromDisjunctiveNormalForm $ reduceEmptyAndUniversal' $ toDisjunctiveNormalForm' adt

  where
    fromDisjunctiveNormalForm :: Disjunct (ADT a) -> ADT a
    fromDisjunctiveNormalForm (Disjunct cjs) = SUM (PROD <<< (\(Conjunct a) -> a) <$> cjs)

    toDisjunctiveNormalForm' :: ADT a -> Disjunct (ADT a)
    toDisjunctiveNormalForm' x@(ST a) = Disjunct [(Conjunct [ST a])]
    toDisjunctiveNormalForm' (SUM ts) = Disjunct $ concat $ (\(Disjunct x) -> x) <<< toDisjunctiveNormalForm' <$> ts
    toDisjunctiveNormalForm' (PROD ts) = Disjunct $ matrixMultiply ((\(Disjunct x) -> x) <<< toDisjunctiveNormalForm' <$> ts)
    toDisjunctiveNormalForm' EMPTY = Disjunct [Conjunct [EMPTY]]
    toDisjunctiveNormalForm' UNIVERSAL = Disjunct [Conjunct [UNIVERSAL]]

    reduceEmptyAndUniversal' :: Disjunct (ADT a) -> Disjunct (ADT a)
    reduceEmptyAndUniversal' (Disjunct cjs) = Disjunct $ catMaybes (x <$> cjs)
      where
        x :: Conjunct (ADT a) -> Maybe (Conjunct (ADT a))
        x (Conjunct as) = if isJust $ elemIndex UNIVERSAL as
          then Nothing
          else if isJust $ elemIndex EMPTY as 
            then Just $ Conjunct $ filter (eq EMPTY) as
            else Just $ Conjunct as

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

-- | The `Reducible` class implements a pattern to recursively process an ADT.
class Reducible a b where
  reduce :: forall m. Monad m => (a -> m b) -> ADT a -> m b

-- | Reduce an `ADT EnumeratedRoleType` with `f :: EnumeratedRoleType -> MP Boolean`
-- | Does **not** take the binding of a role into account.
instance reducibleToBool :: Reducible a Boolean where
  reduce f (ST a) = f a
  reduce f (SUM adts) = do
    (bools :: Array Boolean) <- traverse (reduce f) adts
    pure $ unwrap $ foldMap Conj bools
  reduce f (PROD adts) = do
    (bools :: Array Boolean) <- traverse (reduce f) adts
    pure $ unwrap $ foldMap Disj bools
  reduce f EMPTY = pure false
  reduce f UNIVERSAL = pure true

instance reducibleToString :: Reducible a String where
  reduce f (ST a) = f a
  reduce f (SUM adts) = intercalate ", " <$> traverse (reduce f) adts
  reduce f (PROD adts) = intercalate "+" <$> traverse (reduce f) adts
  reduce f EMPTY = pure ""
  reduce f UNIVERSAL = pure ""

-- | Reduce an `ADT a` with `forall a b m. Monad m => f :: a -> m (Array b)`
-- | `reduce f` then has type `forall a b m. Monad m => ADT a -> m (Array b)`
-- | We treat the UNIVERSAL case as if it were EMPTY (returns []). This is justified by the fact that by specifying
-- | that a role can have no binding, we really say, with respect to properties, that there will be no properties
-- | contributed by the binding.
instance reducibleToArray :: Eq b => Reducible a (Array b) where
  reduce f (ST a) = f a
  reduce f (SUM adts) = do
    (arrays :: Array (Array b)) <- traverse (reduce f) adts
    case uncons arrays of
      Nothing -> pure []
      Just {head, tail} -> pure $ foldl intersect head tail
  reduce f (PROD adts) = do
    (arrays :: Array (Array b)) <- traverse (reduce f) adts
    pure $ foldl union [] arrays
  reduce f EMPTY = pure []
  --
  reduce f UNIVERSAL = pure []

newtype ArrayUnions a = ArrayUnions (Array a)
derive instance Newtype (ArrayUnions a) _
instance Functor ArrayUnions where
  map f (ArrayUnions arr) = ArrayUnions $ (map f arr)
instance Apply ArrayUnions where
  apply (ArrayUnions fs) (ArrayUnions arr) = ArrayUnions (fs <*> arr)
instance Applicative ArrayUnions where 
  pure = ArrayUnions <<< singleton
instance Bind ArrayUnions where
  bind (ArrayUnions arr) f = ArrayUnions $ concat (unwrap <$> (f <$> arr))

instance Eq b => Reducible a (ArrayUnions b) where
  reduce f (ST a) = f a
  reduce f (SUM adts) = do
    (arrays :: Array (ArrayUnions b)) <- traverse (reduce f) adts
    case uncons arrays of
      Nothing -> pure $ ArrayUnions []
      Just {head, tail} -> pure $ foldl (\(ArrayUnions x) (ArrayUnions y) -> ArrayUnions (x `union` y)) head tail
  reduce f (PROD adts) = do
    (arrays :: Array (ArrayUnions b)) <- traverse (reduce f) adts
    pure $ foldl (\(ArrayUnions x) (ArrayUnions y) -> ArrayUnions (x `union` y)) (ArrayUnions []) arrays
  reduce f EMPTY = pure $ ArrayUnions []
  --
  reduce f UNIVERSAL = pure $ ArrayUnions []


-- | Reduce an `ADT a` with `f :: a -> MP (ADT b)`.
-- | `reduce f` then has type `ADT a -> MP (ADT b)`.
-- | Notice that reducing an ADT EnumeratedRoleType is not a transitive operation over binding or aspects!
instance reducibletoADT :: Eq b => Reducible a (ADT b) where
  reduce f (ST et) = f et
  reduce f (SUM adts) = map sum (traverse (reduce f) adts)
  reduce f (PROD adts) = map product (traverse (reduce f) adts)
  reduce f EMPTY = pure EMPTY
  reduce f UNIVERSAL = pure UNIVERSAL

--------------------------------------------------------------------------------------------------
---- COMMONLEAVESINADT
--------------------------------------------------------------------------------------------------
-- | As class Reducible is defined in terms of a Monad, we use Identity.
-- | Part of the semantics is captured by these two rules:
-- | SUM (PROD A B) (PROD B C) --> [B]
-- | PROD A B -> [A, B]
-- | In terms of sets: transform the ADT to Disjunctive Normal Form and then understand it
-- | as an intersection of unions.
commonLeavesInADT :: forall a. Eq a => ADT a -> Array a
commonLeavesInADT = unwrap <<< reduce ((pure <<< singleton) :: a -> Identity (Array a))

--------------------------------------------------------------------------------------------------
---- ALLLEAVESINADT
--------------------------------------------------------------------------------------------------
-- | Collect any leaf occurring in the ADT.
-- | In terms of sets: transform the ADT to Disjunctive Normal Form and then understand it
-- | as an union of unions.
allLeavesInADT :: forall a. Eq a => Ord a => ADT a -> Array a
allLeavesInADT (ST a) = [a]
allLeavesInADT (SUM terms) = nub $ concat $ map allLeavesInADT terms
allLeavesInADT (PROD terms) = nub $ concat $ map allLeavesInADT terms
allLeavesInADT EMPTY = []
allLeavesInADT UNIVERSAL = []

--------------------------------------------------------------------------------------------------
---- SPECIALISESADT
--------------------------------------------------------------------------------------------------
-- | a1 `equalsOrGeneralises` a2
-- | intuitively when a2 is built from a1 (or a2 == a1).
-- | See: Semantics of the Perspectives Language, chapter Another ordering of Role types for an explanation.
equalsOrSpecialises :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
equalsOrSpecialises adt1 adt2 = let
  (adt1' :: ADT a) = toDisjunctiveNormalForm adt1
  adt2' = toDisjunctiveNormalForm adt2
  in
  equalsOrSpecialises_ adt1' adt2'

-- | a1 `equalsOrSpecialises_` a2
-- | intuitively when a2 is built from a1 (or a2 == a1).
equalsOrSpecialises_ :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
equalsOrSpecialises_ a1 a2 = 
  case a1 of 
    EMPTY -> true
    UNIVERSAL -> case a2 of 
      UNIVERSAL -> true
      _ -> false
    l@(ST _) -> case a2 of
      EMPTY -> false
      UNIVERSAL -> true
      r@(ST _) -> l == r
      PROD rs -> [l] == rs
      SUM rs -> isJust $ findIndex (\r -> l `equalsOrSpecialises_` r) rs
    PROD ts -> case a2 of
      EMPTY -> false
      UNIVERSAL -> true
      r@(ST _) -> isJust $ elemIndex r ts
      PROD rs -> ts `superset` rs
      SUM rs -> isJust $ findIndex (\r -> PROD ts `equalsOrSpecialises_` r) rs 
    SUM ls -> foldl (\allTrue l -> if allTrue then l `equalsOrSpecialises_` a2 else false) true ls
  where
  superset :: forall x. Ord x => Eq x => Array x -> Array x -> Boolean
  superset super sub = (fromFoldable sub) `subset` (fromFoldable super)

generalises :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
generalises adt1 adt2 = adt1 /= adt2 && adt1 `equalsOrGeneralises` adt2

specialises :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
specialises = flip generalises

equalsOrGeneralises :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
equalsOrGeneralises = flip equalsOrSpecialises
