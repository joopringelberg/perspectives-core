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

import Data.Array (concat, fold, intercalate, intersect, null, uncons, union)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldlDefault, foldrDefault)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (unwrap)
import Data.Traversable (class Traversable, sequenceDefault, traverse)
import Kishimen (genericSumToVariant, variantToGenericSum)
import Partial.Unsafe (unsafePartial)
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Eq, class Functor, class HeytingAlgebra, class Ord, class Show, bind, disj, map, not, pure, show, ($), (<#>), (<$>), (<<<), (<>))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

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

instance x :: (Show a, Show label) => PrettyPrint (ADT label a) where 
  prettyPrint' t a@(ST _) = t <> show a
  prettyPrint' t a@(UET _) = t <> show a
  prettyPrint' t a@(CT _ _) = t <> show a
  prettyPrint' t (PROD terms) = t <> "PROD\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)
  prettyPrint' t (SUM terms) = t <> "SUM\n" <> (intercalate "\n" $ prettyPrint' (t <> "  ") <$> terms)

instance eqADT :: (Eq a, Eq label) => Eq (ADT label a) where
  eq b1 b2 = genericEq b1 b2

derive instance ordADT :: (Ord a, Ord label) => Ord (ADT label a)

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

-- This function is like Foldable foldMap, but it folds with Conj over PROD and with Disj over SUM.
foldMapADT :: forall a label h. HeytingAlgebra h => (a -> h) -> ADT label a -> h
foldMapADT f adt = case adt of
  ST a -> f a
  UET a -> f a
  CT label a -> foldMapADT f a
  PROD as -> unwrap $ fold (Conj <<< foldMapADT f <$> as)
  SUM as -> unwrap $ fold (Disj <<< foldMapADT f <$> as)

instance (WriteForeign a, WriteForeign label) => WriteForeign (ADT label a) where
  writeImpl f = writeImpl( genericSumToVariant f)

instance (ReadForeign a, ReadForeign label) => ReadForeign (ADT label a) where
  readImpl f = map variantToGenericSum (readImpl f)

-- To be applied to an expanded tree only. In the expanded tree, UET will not occur.
toDisjunctiveNormalForm :: forall a label. Eq a => ADT label a -> ADT label a
toDisjunctiveNormalForm adt = case adt of 
  ST a -> SUM [(PROD [ST a])]
  -- Expand first; then this case will not occur.
  UET a -> SUM [(PROD [UET a])]
  -- In the disjunctive normal form we have neither UET, nor CT.
  -- We have a tree built from ST, SUM and PROD.
  CT label a -> toDisjunctiveNormalForm a
  SUM as -> unsafePartial flattenSums $ map toDisjunctiveNormalForm as
  PROD as -> unsafePartial distribute (map toDisjunctiveNormalForm as)

  where
    -- the argument is the product of applying toDisjunctiveNormalForm to an array of ADT - so it must
    -- be an array of SUM (PROD ST)
    flattenSums :: Partial => Array (ADT label a) -> ADT label a
    flattenSums sums = SUM $ concat (sums <#>
      (\a -> case a of 
        SUM ds -> ds))

    -- Because of context we can safely assume that adts is an Array of (SUM [(PROD [ST])])
    distribute :: Partial => Array (ADT label a) -> ADT label a
    distribute adts = SUM $ (matrixMultiply $ adts <#> (\sum -> case sum of
      SUM products -> products <#> (\product -> case product of 
        PROD ps -> Conjunct ps))) <#> \(Conjunct terms) -> PROD terms

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

computeBoolean :: forall label a. (a -> Boolean) -> ADT label a -> Boolean
computeBoolean f = foldMapADT f

computeCollection :: forall label a b. Eq b => (a -> Array b) -> ADT label a -> Array b
computeCollection f = unsafePartial fromHArray <<< foldMapADT (HArray <<< f)

{-
-- | SUM with simplifications:
-- | SUM [EMPTY, ..] = EMPTY
-- | SUM [UNIVERSAL, ..] = [..]
-- | SUM [SUM [..], x] = SUM [x, ..]
-- | SUM [SUM [..], SUM[..]] = SUM [.. ..]
-- | SUM [x, .. x] = SUM[.., x]
-- | SUM [x] = x
sum :: forall a. Eq a => Array (ADT a) -> ADT a
sum = foldl sum' UNIVERSAL >>> \(s :: ADT a) -> case s of
    SUM y | length y == 1 -> unsafePartial $ AP.head y
    otherwise -> otherwise
  where
  sum' :: ADT a -> ADT a -> ADT a
  sum' EMPTY _ = EMPTY
  sum' UNIVERSAL y = y
  sum' (SUM terms1) (SUM terms2) = SUM (union terms1 terms2)
  sum' (SUM terms) y = SUM (union terms [y])
  sum' t1@(ST p) t2@(ST y) = if p == y then t1 else SUM [t1, t2]
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
    PROD y | length y == 1 -> unsafePartial $ AP.head y
    otherwise -> otherwise
  where
  prod' :: ADT a -> ADT a -> ADT a
  prod' UNIVERSAL _ = UNIVERSAL
  prod' EMPTY y = y
  prod' (PROD terms1) (PROD terms2) = PROD (union terms1 terms2)
  prod' (PROD terms) y = PROD (union terms [y])
  prod' t1@(ST p) t2@(ST y) = if p == y then t1 else PROD [t1, t2]
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
    toDisjunctiveNormalForm' (ST a) = Disjunct [(Conjunct [ST a])]
    toDisjunctiveNormalForm' (SUM ts) = Disjunct $ concat $ (\(Disjunct y) -> y) <<< toDisjunctiveNormalForm' <$> ts
    toDisjunctiveNormalForm' (PROD ts) = Disjunct $ matrixMultiply ((\(Disjunct y) -> y) <<< toDisjunctiveNormalForm' <$> ts)
    toDisjunctiveNormalForm' EMPTY = Disjunct [Conjunct [EMPTY]]
    toDisjunctiveNormalForm' UNIVERSAL = Disjunct [Conjunct [UNIVERSAL]]

    reduceEmptyAndUniversal' :: Disjunct (ADT a) -> Disjunct (ADT a)
    reduceEmptyAndUniversal' (Disjunct cjs) = reduceSum $ Disjunct (reduceProduct <$> cjs)
      where
        reduceProduct :: Conjunct (ADT a) -> Conjunct (ADT a)
        reduceProduct (Conjunct as) = let
            -- PROD T UNIVERSAL = T
            as' = filter (notEq UNIVERSAL) as
          in
            -- PROD T EMPTY = EMPTY
            if isJust $ elemIndex EMPTY as'
              then Conjunct [EMPTY]
              else if null as' 
                -- Ensure the canonical form.
                then Conjunct [UNIVERSAL]
                else Conjunct as'
        
        reduceSum :: Disjunct (ADT a) -> Disjunct (ADT a)
        reduceSum (Disjunct conjuncts) = let
            -- SUM T EMPTY = T
            conjuncts' = filter (\(Conjunct cjs') -> isNothing $ elemIndex EMPTY cjs') conjuncts
          in
            -- SUM T UNIVERSAL = UNIVERSAL
            if isJust $ findIndex (\(Conjunct cjs') -> isJust $ elemIndex UNIVERSAL cjs') conjuncts
              then Disjunct [Conjunct [UNIVERSAL]]
              else if null conjuncts' 
                -- Ensure the canonical form.
                then Disjunct [Conjunct [EMPTY]]
                else Disjunct conjuncts'

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
      Just {head, tail} -> pure $ foldl (\(ArrayUnions p) (ArrayUnions y) -> ArrayUnions (p `union` y)) head tail
  reduce f (PROD adts) = do
    (arrays :: Array (ArrayUnions b)) <- traverse (reduce f) adts
    pure $ foldl (\(ArrayUnions p) (ArrayUnions y) -> ArrayUnions (p `union` y)) (ArrayUnions []) arrays
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
-- | a1 `equalsOrGeneralisesADT` a2
-- | intuitively when a2 is built from a1 (or a2 == a1).
-- | See: Semantics of the Perspectives Language, chapter Another ordering of Role types for an explanation.
equalsOrSpecialisesADT :: forall a. Ord a => Eq a => ADT a -> ADT a -> Boolean
equalsOrSpecialisesADT adt1 adt2 = let
  (adt1' :: ADT a) = toDisjunctiveNormalForm adt1
  adt2' = toDisjunctiveNormalForm adt2
  in
  equalsOrSpecialisesADT_ adt1' adt2'

-- | a1 `equalsOrSpecialisesADT_` a2
-- | intuitively when a2 is built from a1 (or a2 == a1).
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
      r@(ST _) -> isJust $ elemIndex r ts
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
-}
