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

module Perspectives.Models

where

import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, (##=))
import Perspectives.Identifiers (modelUri2DomeinFileName_)
import Perspectives.Instances.ObjectGetters (getEnumeratedRoleInstances)
import Perspectives.ModelDependencies (modelExternalModelIdentification)
import Perspectives.ModelDependencies (modelsInUse) as DEP
import Perspectives.Names (getMySystem)
import Perspectives.Query.UnsafeCompiler (getDynamicPropertyGetter)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..), DomeinFileId(..))
import Prelude (bind, pure, ($), (<$>), (<<<), (>=>))

modelsInUseRole :: EnumeratedRoleType
modelsInUseRole = EnumeratedRoleType DEP.modelsInUse

-- | Returns an array of DomeinFileIds (not the modelURNs)
modelsInUse :: MonadPerspectives (Array DomeinFileId)
modelsInUse = do
  system <- getMySystem
  propertyGetter <- getDynamicPropertyGetter
    modelExternalModelIdentification
    (ST modelsInUseRole)
  values <- (ContextInstance system) ##= (getEnumeratedRoleInstances modelsInUseRole >=> propertyGetter)
  pure $ DomeinFileId <<< unsafePartial modelUri2DomeinFileName_ <<< unwrap <$> values
