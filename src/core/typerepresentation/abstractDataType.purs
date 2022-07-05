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

import Data.Array (concat, cons, elemIndex, findIndex, intercalate, intersect, length, nub, singleton, uncons, union)
import Data.Array.Partial (head) as AP
import Data.Eq.Generic (genericEq)
import Data.Foldable (foldMap, foldl)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.Set (fromFoldable, subset)
import Data.Traversable (traverse)
import Foreign.Class (class Decode, class Encode)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Functor, class Monad, class Ord, class Show, bind, flip, map, pure, show, ($), (&&), (/=), (<$>), (<<<), (<>), (==), (>>>))
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

instance eqADT :: (Eq a) => Eq (ADT a) where
  eq b1 b2 = genericEq b1 b2

instance writeForeignADT :: WriteForeign a => WriteForeign (ADT a) where
  writeImpl f = writeImpl( genericSumToVariant f)

instance readForeightADT :: ReadForeign a => ReadForeign (ADT a) where
  readImpl f = map variantToGenericSum (readImpl f)

instance encodeADT :: (Encode a) => Encode (ADT a) where
  encode q = genericEncode defaultOptions q

instance decodeADT :: (Decode a) => Decode (ADT a) where
  decode q = genericDecode defaultOptions q

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

data ADT' a = ST' a | SUM' (ADT' a) (ADT' a) | PROD' (ADT' a) (ADT' a) | EMPTY' | UNIVERSAL'

derive instance Generic (ADT' a) _
instance (Eq a) => Eq (ADT' a) where
  eq b1 b2 = genericEq b1 b2

toDNF :: forall a. ADT' a -> ADT' a
toDNF (PROD' p (SUM' s1 s2)) = SUM' (toDNF (PROD' p s1)) (toDNF (PROD' p s2))
toDNF (PROD' (SUM' s1 s2) p) = SUM' (toDNF (PROD' p s1)) (toDNF (PROD' p s2))
toDNF x = x

toADT' :: forall a. ADT a -> ADT' a
toADT' (ST a) = ST' a
toADT' EMPTY = EMPTY'
toADT' UNIVERSAL = UNIVERSAL'
toADT' (SUM as) = case uncons as of 
  Just {head, tail} -> foldl (\adt' adt -> SUM' (toADT' adt) adt') (toADT' head) tail
  Nothing -> EMPTY'
toADT' (PROD as) = case uncons as of 
  Just {head, tail} -> foldl (\adt' adt -> PROD' (toADT' adt) adt') (toADT' head) tail
  Nothing -> EMPTY' 

toADT :: forall a. Eq a => ADT' a -> ADT a
toADT (ST' a) = ST a
toADT EMPTY' = EMPTY
toADT UNIVERSAL' = UNIVERSAL
toADT (SUM' a as) = let
  adt = toADT a
  in unsafePartial case toADT as of
    SUM ys -> if isJust $ elemIndex adt ys
      then SUM ys
      else SUM $ cons adt ys
    y -> SUM [adt, y]
toADT (PROD' a as) = let
  adt = toADT a
  in unsafePartial case toADT as of
    PROD ys -> if isJust $ elemIndex adt ys
      then PROD ys
      else PROD $ cons adt ys
    y -> PROD [adt, y]

toLeftAssociative :: forall a. Eq a => ADT' a -> ADT' a
toLeftAssociative (SUM' (SUM' a b ) c) = SUM' a $ toLeftAssociative (SUM' b c)
toLeftAssociative (PROD' (PROD' a b ) c) = PROD' a $ toLeftAssociative(PROD' b c)
toLeftAssociative x = x

reduceEmptyAndUniversal :: forall a. Eq a => ADT' a -> ADT' a
reduceEmptyAndUniversal (PROD' x EMPTY') = reduceEmptyAndUniversal x
reduceEmptyAndUniversal (PROD' EMPTY' x) = reduceEmptyAndUniversal x
reduceEmptyAndUniversal (PROD' _ UNIVERSAL') = UNIVERSAL'
reduceEmptyAndUniversal (PROD' UNIVERSAL' _) = UNIVERSAL'

reduceEmptyAndUniversal (SUM' _ EMPTY') = EMPTY'
reduceEmptyAndUniversal (SUM' EMPTY' _) = EMPTY'
reduceEmptyAndUniversal (SUM' x UNIVERSAL') = x
reduceEmptyAndUniversal (SUM' UNIVERSAL' x) = x

reduceEmptyAndUniversal (PROD' x y) = let
  r = reduceEmptyAndUniversal y
  in
    if x == r
      then x
      else PROD' x r
reduceEmptyAndUniversal (SUM' x y) = let
  r = reduceEmptyAndUniversal y
  in
    if x == r
      then x
      else SUM' x r
reduceEmptyAndUniversal x = x

-- If adt is a SUM, that sum is in normal form (its members are products).
toDisjunctiveNormalForm :: forall a. Eq a => ADT a -> ADT a
toDisjunctiveNormalForm adt = let 
  adt' = toADT' adt
  leftA = toLeftAssociative adt'
  reduced = reduceEmptyAndUniversal leftA
  dnf = toDNF reduced
  adtOut = toADT dnf
  in adtOut

-- | From two ADTs, compute an ADT that represents their commonality.
-- | The result is simplified according to these rules:
-- | SUM [EMPTY, ..] = EMPTY
-- | SUM [UNIVERSAL, ..] = [..]
-- | SUM [ST x, SUM [..] ] = SUM [x, ..]
-- | SUM [SUM [..], SUM[..]] = SUM [.. ..]
-- | SUM [x, .. x] = SUM[.., x]
-- | SUM [x] = x
-- | PRODUCT [UNIVERSAL, ..] = UNIVERSAL
-- | PRODUCT [EMPTY, ..] = [..]
-- | PRODUCT [PRODUCT[..], ST x] = PRODUCT[.., x]
-- | PRODUCT [PRODUCT[..], PRODUCT[..]] = PRODUCT[.. ..]
-- | PRODUCT [x, .. x] = PRODUCT[.., x]
-- | PRODUCT [x] = x
-- intersectionOfADT :: forall a. Eq a => ADT a -> ADT a -> ADT a
-- -- 5 cases
-- intersectionOfADT EMPTY _ = EMPTY
-- -- 5 cases, total is 10
-- intersectionOfADT UNIVERSAL x = x
-- -- 1 case, total is 11
-- intersectionOfADT t@(ST x) (ST y) = if x == y then t else EMPTY
-- -- 1 case, total is 12
-- intersectionOfADT (ST x) (SUM terms) = EMPTY
-- -- 1 case, total is 13
-- intersectionOfADT t@(ST _) (PROD terms) = if isJust $ elemIndex t terms then t else EMPTY
-- -- 1 case, total is 14. 2 ST cases missing: ST UNIVERSAL and ST EMPTY. Covered by flip in last case.
-- intersectionOfADT (PROD terms1) (PROD terms2) = let i = intersect terms1 terms2 in
--   if null i then EMPTY else (product i)
-- -- 1 case, total is 15
-- intersectionOfADT (PROD terms) t2@(SUM _) = if isJust $ elemIndex t2 terms then t2 else EMPTY
-- -- 1 case, total is 16, 3 PROD cases missing: PROD ST, PROD UNIVERSAL, PROD EMPTY. Covered by flip in last case.
-- intersectionOfADT (SUM terms1) (SUM terms2) = sum (union terms1 terms2)
-- -- 4 SUM cases missing, 9 missing in total: SUM ST, SUM PROD, SUM UNIVERSAL, SUM EMPTY. Covered by flip in last case.
-- intersectionOfADT a b = intersectionOfADT b a
--
-- -- | From two ADT's compute an ADT that represents their union
-- unionOfADT :: forall a. Eq a => ADT a -> ADT a -> ADT a
-- unionOfADT EMPTY x = x
-- unionOfADT UNIVERSAL _ = UNIVERSAL
-- unionOfADT t1@(ST _) t2@(ST _) = product [t1, t2]
-- unionOfADT t1@(ST _) t2@(SUM _) = product [t1, t2]
-- unionOfADT t1@(ST _) t2@(PROD _) = product [t2, t1]
-- unionOfADT t1@(PROD _) t2@(PROD _) = product [t1, t2]
-- unionOfADT t1@(PROD _) t2@(SUM _) = product [t1, t2]


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
-- | a1 `equalsOrGeneralisesADT` a2
-- | intuitively when a2 is built from a1 (or a2 == a1).
-- | See: Semantics of the Perspectives Language, chapter Another ordering of Role types for an explanation.
equalsOrSpecialisesADT :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
equalsOrSpecialisesADT adt1 adt2 = let
  (adt1' :: ADT a) = toDisjunctiveNormalForm adt1
  adt2' = toDisjunctiveNormalForm adt2
  in
  equalsOrSpecialisesADT_ adt1' adt2'

equalsOrSpecialisesADT_ :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
equalsOrSpecialisesADT_ a1 a2 = 
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
      SUM rs -> isJust $ findIndex (\r -> l `equalsOrSpecialisesADT_` r) rs
    PROD ts -> case a2 of
      EMPTY -> false
      UNIVERSAL -> true
      r@(ST _) -> ts == [r]
      PROD rs -> ts `superset` rs
      SUM rs -> isJust $ findIndex (\r -> PROD ts `equalsOrSpecialisesADT_` r) rs 
    SUM ls -> foldl (\allTrue l -> if allTrue then l `equalsOrSpecialisesADT_` a2 else false) true ls
  where
  superset :: forall x. Ord x => Eq x => Array x -> Array x -> Boolean
  superset super sub = (fromFoldable sub) `subset` (fromFoldable super)

generalisesADT :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
generalisesADT adt1 adt2 = adt1 /= adt2 && adt1 `equalsOrGeneralisesADT` adt2

specialisesADT :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
specialisesADT = flip generalisesADT

equalsOrGeneralisesADT :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
equalsOrGeneralisesADT = flip equalsOrSpecialisesADT
