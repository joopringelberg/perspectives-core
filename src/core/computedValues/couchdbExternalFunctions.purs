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
import Perspectives.CoreTypes (MP, assumption, MPQ)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.External.CoreFunctionsCache (ExternalFunction, externalFunctionInsert)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance(..))
import Prelude (class Monad, Unit, discard, map, pure, ($), (<<<), (<>), (>>=))
import Unsafe.Coerce (unsafeCoerce)

models :: MPQ RoleInstance
models = ArrayT do
  tell [assumption "model:User$MijnSysteem" ophaalTellerName]
  lift $ getListOfModels

  where
    getListOfModels :: MP (Array RoleInstance)
    getListOfModels = catchError ((documentNamesInDatabase "perspect_models") >>= pure <<< map (RoleInstance <<< (_ <> "$External"))) \_ -> pure []

    ophaalTellerName :: String
    ophaalTellerName = "model:System$PerspectivesSystem$External$ModelOphaalTeller"

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.CoreFunctionsCache.lookupExternalFunction`.
externalFunctions :: Array (Tuple String ExternalFunction)
externalFunctions = [
  Tuple "couchdb_Models" {func: unsafeCoerce models, nArgs: 0}
]

addExternalFunctions :: forall m. Monad m => m Unit
addExternalFunctions = for_ externalFunctions \(Tuple n f) -> pure $ externalFunctionInsert n f.func f.nArgs
