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

-- | Conceptually, an `Environment a` is a stack of frames of a, where a is a Foreign.Object.
-- | A variable binding is added to the frame at the top of the stack.
-- | A variable is searched from the top of the stack to the bottom. The value found in the
-- | first frame with the variable name is returned.
-- | This causes variable shadowing.

module Perspectives.Instances.Environment where

import Data.Function.Uncurried (Fn1, Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Prelude (class Eq, class Show, eq, show, ($))

foreign import data Environment :: Type -> Type

foreign import empty :: forall a. Environment a

foreign import _lookup :: forall a z. Fn4 z (a -> z) String (Environment a) z

lookup :: forall a. String -> Environment a -> Maybe a
lookup = runFn4 _lookup Nothing Just

foreign import _pushFrame :: forall a. Fn1 (Environment a) (Environment a)

foreign import _addVariable :: forall a. Fn3 String a (Environment a) (Environment a)

addVariable :: forall a. String -> a -> Environment a -> Environment a
addVariable = runFn3 _addVariable

foreign import _toObjectArray :: forall a. Fn1 (Environment a) (Array (Object a))

foreign import _fromObjectArray :: forall a. Fn1 (Array (Object a)) (Environment a)

instance showEnvironment :: Show a => Show (Environment a) where
  show e = show $ _toObjectArray e

instance eqEnvironment :: Eq a => Eq (Environment a) where
  eq e1 e2 = eq (_toObjectArray e1) (_toObjectArray e2)
