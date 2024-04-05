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


module Perspectives.Persistence.RunEffectAff

where

import Effect.Aff.Compat (EffectFn3, EffectFnAff)
import Effect.Uncurried (EffectFn2, EffectFn4, EffectFn5, EffectFn6)

-----------------------------------------------------------
-- RUNEFFECTFNAFF2
-----------------------------------------------------------
-- | With `fromEffectFnAff`, we apply Effect functions of a single argument to Aff.
-- | Such functions must return a `EffectFnAff`. As an example:
-- |  `foreign import deleteDatabaseImpl :: PouchdbDatabase -> EffectFnAff Foreign`
-- | And then:
-- |  `fromEffectFnAff $ deleteDatabaseImpl db`
-- | However, in this module we have various foreign functions that have more than one
-- | parameter. `addDocumentImpl` is an example:
-- |  `foreign import addDocumentImpl :: EffectFn2 PouchdbDatabase Foreign Foreign`
-- | We cannot apply `fromEffectFnAff` directly to functions of this type.
-- | `runEffectFnAff2` comes to the rescue, allowing you to write:
-- |  addDocumentImpl' :: PouchdbDatabase -> Foreign -> EffectFnAff Foreign
-- |  addDocumentImpl' = `runEffectFnAff2 addDocumentImpl`
-- | And then:
-- |  `fromEffectFnAff $ addDocumentImpl' db doc`
foreign import runEffectFnAff2 :: forall a b r.
  EffectFn2 a b r -> a -> b -> EffectFnAff r

-----------------------------------------------------------
-- RUNEFFECTFNAFF3
-----------------------------------------------------------
foreign import runEffectFnAff3 :: forall a b c r.
  EffectFn3 a b c r -> a -> b -> c -> EffectFnAff r

-----------------------------------------------------------
-- RUNEFFECTFNAFF4
-----------------------------------------------------------
foreign import runEffectFnAff4 :: forall a b c d r.
  EffectFn4 a b c d r -> a -> b -> c -> d -> EffectFnAff r

-----------------------------------------------------------
-- RUNEFFECTFNAFF5
-----------------------------------------------------------
foreign import runEffectFnAff5 :: forall a b c d e r.
  EffectFn5 a b c d e r -> a -> b -> c -> d -> e -> EffectFnAff r

-----------------------------------------------------------
-- RUNEFFECTFNAFF6
-----------------------------------------------------------
foreign import runEffectFnAff6 :: forall a b c d e f r.
  EffectFn6 a b c d e f r -> a -> b -> c -> d -> e -> f -> EffectFnAff r
