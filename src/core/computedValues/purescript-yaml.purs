  -- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

-- A very incomplete wrapper for js-yaml: https://github.com/nodeca/js-yaml

module Purescript.YAML where

import Control.Monad.Error.Class (try)
import Data.Either (Either)
import Effect (Effect)
import Effect.Exception (Error)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import loadImpl :: forall a. EffectFn1 String a

-- | From a YAML source, produce JSON.
load :: forall a. String -> Effect (Either Error a)
load source = try (runEffectFn1 loadImpl source)

foreign import dumpImpl :: forall a. EffectFn1 a String

-- | From a JSON source, produce a YAML string.
dump :: forall a. a -> Effect (Either Error String)
dump a = try (runEffectFn1 dumpImpl a)