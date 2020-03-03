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

module Perspectives.SetupUser where

import Data.Maybe (Maybe(..))
import Perspectives.ContextAndRole (changeContext_me, changeRol_isMe)
import Perspectives.CoreTypes (MonadPerspectives, (##>>))
import Perspectives.Extern.Couchdb (addModelToLocalStore)
import Perspectives.InstanceRepresentation (PerspectContext)
import Perspectives.Instances.ObjectGetters (getRole)
import Perspectives.Persistent (entitiesDatabaseName, getPerspectContext, getPerspectRol, saveEntiteit_, tryGetPerspectEntiteit)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType(..))
import Perspectives.RunMonadPerspectivesTransaction (runSterileTransaction)
import Perspectives.SetupCouchdb (setRoleView)
import Perspectives.User (getCouchdbBaseURL)
import Prelude (Unit, bind, pure, unit, void, ($), discard, (<>), (>>=))

modelDirectory :: String
modelDirectory = "./src/model"

-- | Set up by adding model:System to the users' models database. This will add the model instances, too.
-- | This function also ensures CURRENTUSER.
setupUser :: MonadPerspectives Unit
setupUser = do
  (mu :: Maybe PerspectContext) <- tryGetPerspectEntiteit (ContextInstance "model:User$MijnSysteem")
  case mu of
    Nothing -> do
      -- First, upload model:System to perspect_models.
      cdbUrl <- getCouchdbBaseURL
      void $ runSterileTransaction (addModelToLocalStore [cdbUrl <> "/repository/model:System"])
      -- now set isMe of "model:User$MijnSysteem$User_0001" to true.
      user <- ContextInstance "model:User$MijnSysteem" ##>> getRole (EnumeratedRoleType "model:System$PerspectivesSystem$User")
      userRol <- getPerspectRol user
      void $ saveEntiteit_ user (changeRol_isMe userRol true)
      -- And set 'me' of "model:User$MijnSysteem"
      (mijnSysteem :: PerspectContext) <- getPerspectContext (ContextInstance "model:User$MijnSysteem")
      void $ saveEntiteit_ (ContextInstance "model:User$MijnSysteem") (changeContext_me mijnSysteem (Just user))
      entitiesDatabaseName >>= setRoleView

    otherwise -> pure unit
