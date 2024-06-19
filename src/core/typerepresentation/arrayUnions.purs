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

module Perspectives.ArrayUnions  where

import Prelude

import Data.Array (concat, singleton)
import Data.Newtype (class Newtype, unwrap)

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
instance Semigroup (ArrayUnions a) where
  append (ArrayUnions a1) (ArrayUnions a2) = ArrayUnions (a1 <> a2)
