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

module Perspectives.Representation.CNF where

import Data.Array (concat, intercalate, nub, null)
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Set (fromFoldable) as SET
import Partial.Unsafe (unsafePartial)
import Perspectives.Representation.ExpandedADT (ExpandedADT(..))
import Perspectives.Utilities (class PrettyPrint, prettyPrint')
import Prelude (class Eq, class Ord, class Show, map, show, ($), (<#>), (<$>), (<>), (==))
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)


--------------------------------------------------------------------------------------------------
---- CONJUNCTIVE NORMAL FORM
--------------------------------------------------------------------------------------------------
type CNF = DPROD
data DPROD a = DPROD (Array (DSUM a))
data DSUM a = DSUM (Array a)

derive instance Generic (DSUM a) _
instance (WriteForeign a) => WriteForeign (DSUM a) where
  writeImpl (DSUM as) = writeImpl as
instance (ReadForeign a) => ReadForeign (DSUM a) where
  readImpl f = DSUM <$> readImpl f
instance (Show a) => Show (DSUM a) where
  show (DSUM adts) = "(" <> "DSUM" <> show adts <> ")"
instance (Show a, PrettyPrint a) => PrettyPrint (DSUM a) where 
  prettyPrint' t (DSUM terms) = t <> "(DSUM [\n" <> (intercalate ",\n" $ prettyPrint' (t <> "  ") <$> terms) <> "\n" <> t <> "  ])"
instance (Ord a) => Eq (DSUM a) where
  eq (DSUM left) (DSUM right) = SET.fromFoldable left == SET.fromFoldable right
derive instance (Ord a) => Ord (DSUM a)
  
derive instance Generic (DPROD a) _

instance (Eq a, Ord a) => Eq (DPROD a) where
  eq (DPROD left) (DPROD right) = SET.fromFoldable left == SET.fromFoldable right

derive instance (Ord a) => Ord (DPROD a)

instance (Show a) => Show (DPROD a) where
  show (DPROD adts) = "(" <> "DPROD" <> show adts <> ")"

instance (Show a, PrettyPrint a) => PrettyPrint (DPROD a) where 
  prettyPrint' t (DPROD terms) = t <> "(DPROD [\n" <> (intercalate ",\n" $ prettyPrint' (t <> "  ") <$> terms) <> "\n" <> t <> "  ])"

instance (WriteForeign a) => WriteForeign (DPROD a) where
  writeImpl (DPROD sums) = writeImpl sums

instance (ReadForeign a) => ReadForeign (DPROD a) where
  readImpl f = DPROD <$> readImpl f

-- To be applied to an expanded tree only. In the expanded tree, UET will not occur.
toConjunctiveNormalForm :: forall a. Eq a => Ord a => ExpandedADT a -> DPROD a
toConjunctiveNormalForm adt = case adt of 
  EST a -> DPROD [(DSUM [a])]
  -- In the disjunctive normal form we have no UET.
  -- We have a tree built from EST, ECT, ESUM and EPROD.
  -- We can safely ignore the label; it is in a, too.
  ECT label a -> toConjunctiveNormalForm a
  EPROD as -> unsafePartial flattenProducts (map toConjunctiveNormalForm as)
  ESUM as -> unsafePartial distribute $ map toConjunctiveNormalForm as

-- the argument is the product of applying toDisjunctiveNormalForm to an array of ADT - so it must
-- be an array of DPROD (Array (DSUM a))
flattenProducts :: forall a. Ord a => Partial => Array (DPROD a) -> DPROD a
flattenProducts sums = DPROD $ nub $ concat (sums <#>
  (\a -> case a of 
    DPROD ds -> ds))

-- | From an array of DPRODs that are in a disjunction, create a DPROD.
distribute :: forall a. Ord a => Array (DPROD a) -> DPROD a
distribute dprods = DPROD
  -- If we can combine two DPROD's that are in a disjunction into a single one, we can handle an entire array of them by folding:
  (foldl
    -- Fold with a function that, from a disjunction of two DPROD's, produces a single DPROD.
    (\sumsOfResult (DPROD sumsOfNextprod) -> if null sumsOfResult
      then sumsOfNextprod
      -- The next product might consist of 1 or more DSUM's that are conjuncted together.
      -- If we can construct a DPROD from a single DSUM with sumsOfResult, we can fold over all DSUMs.
      -- CHECK IF FOLDL IS APPROPRIATE
      else (foldl
        -- Fold with a function that from a single DSUM and a DPROD produces a DPROD.
        (\(resultingsums :: Array (DSUM a)) (DSUM nextsum) -> if null sumsOfResult
          then [DSUM nextsum] 
          else let
          -- Form a sum of nextsum and each of the sums in resultingProd.
            (x :: Array (DSUM a)) = (sumsOfResult <#> (\(DSUM sum) -> DSUM $ nub (nextsum <> sum)))
          in 
            resultingsums <> x
          )
        -- Start with an empty array of sums.
        []
        -- Fold over all DSUMS in the nextprod.
        sumsOfNextprod))
    -- Start with an empty array of sums.
    []
    -- Fold over all DPROD elements in the array.
    dprods)
