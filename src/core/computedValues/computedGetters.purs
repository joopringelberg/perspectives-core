-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- Copyright (C) 2019 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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
-- Full text of this license can be found in the LICENSE file in the projects root.

-- END LICENSE

-- | This module defines External Core functions for model:Couchdb.

module Perspectives.Extern.Couchdb where

import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (tell)
import Data.Foldable (for_)
import Data.Tuple (Tuple(..))
import Perspectives.CoreTypes (type (~~>), MP, RoleGetter, assumption)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.ObjectGetterLookup (ComputedFunction, roleGetterCacheInsert)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..))
import Prelude (Unit, discard, map, pure, ($), (<<<), (<>), (>>=))

models :: ContextInstance ~~> RoleInstance
models c = ArrayT do
  tell [assumption "model:User$MijnSysteem" ophaalTellerName]
  lift $ getListOfModels

  where
    getListOfModels :: MP (Array RoleInstance)
    getListOfModels = catchError ((documentNamesInDatabase "perspect_models") >>= pure <<< map (RoleInstance <<< (_ <> "$External"))) \_ -> pure []

    ophaalTellerName :: String
    ophaalTellerName = "model:System$PerspectivesSystem$External$ModelOphaalTeller"

-- | An Array of RoleGetters. Each RoleGetter is inserted into the RoleGetterCache and can be retrieved
-- | with `Perspectives.ObjectGetterLookup.lookupRoleGetterByName`.
computedRoleGetters :: Array (Tuple String (ComputedFunction RoleGetter))
computedRoleGetters = [
  Tuple "couchdb_models" {func: models, functional: false, mandatory: true}
]

addComputedTripleGetters :: MP Unit
addComputedTripleGetters = for_ computedRoleGetters \(Tuple n f) -> pure $ roleGetterCacheInsert n f.func f.functional f.mandatory
