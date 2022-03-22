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

module Perspectives.Checking.Authorization where

import Prelude

import Control.Monad.State (StateT, get)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, (##>>), (###>>))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Instances.ObjectGetters (roleType, typeOfSubjectOfAction)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Class.Role (adtOfRole, getRole)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.Perspective (isPerspectiveOnADT, perspectiveSupportsRoleVerb)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..), RoleType(..))
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerb)
import Perspectives.Types.ObjectGetters (findPerspective, hasPerspectiveOnPropertyWithVerb, hasPerspectiveOnRoleWithVerbs)
import Perspectives.TypesForDeltas (SubjectOfAction)

type Found a = StateT Boolean MonadPerspectives a

hasBeenFound :: Found Boolean
hasBeenFound = get

-- | True iff the user role represented by the SubjectOfAction argument has a perspective
-- | on the role type (in any state) with PropertyVerbs
-- |  * whose properties include the property,
-- |  * whose verbs include the given Verb.
roleHasPerspectiveOnPropertyWithVerb :: SubjectOfAction -> RoleInstance -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnPropertyWithVerb subject roleInstance property verb' = do
  (subjectType :: RoleType) <- typeOfSubjectOfAction subject
  roleType' <- roleInstance ##>> roleType
  (unsafePartial $ hasPerspectiveOnPropertyWithVerb subjectType roleType' property verb') >>=
    if _
      then pure $ Right true
      else pure $ Left $ UnauthorizedForProperty "Auteur" subjectType (ENR roleType') (ENP property) verb'

-- | True if the user role represented by the SubjectOfAction argument has a perspective
-- | on the role type that includes an Action with the given Verb,
-- | OR if the role type has the aspect "sys:RootContext$RootUser"
roleHasPerspectiveOnRoleWithVerb :: SubjectOfAction -> EnumeratedRoleType -> Array RoleVerb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnRoleWithVerb subject roleType verbs = do
  (subjectType :: RoleType) <- typeOfSubjectOfAction subject
  hasPerspective <- unsafePartial (roleType ###>> hasPerspectiveOnRoleWithVerbs verbs subjectType)
  if hasPerspective
    then pure $ Right true
    else pure $ Left $ UnauthorizedForRole "Auteur" subjectType (ENR roleType) verbs

-- | This function differs from `roleHasPerspectiveOnRoleWithVerb` in that it uses an
-- | `authorizedRole` (second parameter). This is the role that binds the external role that we scrutinize,
-- | or it is the Calculated role that results in external roles.
roleHasPerspectiveOnExternalRoleWithVerb :: SubjectOfAction -> Maybe RoleType -> RoleVerb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnExternalRoleWithVerb subject mroleType verb' = case mroleType of
  Nothing -> do
    (subjectType :: RoleType) <- typeOfSubjectOfAction subject
    liftEffect $ logPerspectivesError $ Custom "roleHasPerspectiveOnExternalRoleWithVerb: no authorizedRole provided to construct external role"
    pure $ Left $ Custom "roleHasPerspectiveOnExternalRoleWithVerb: no authorizedRole provided to construct external role"
  Just rt -> do
    (subjectType :: RoleType) <- typeOfSubjectOfAction subject
    (hasPerspectiveWithVerb subjectType rt) >>=
      if _
        then pure $ Right true
        else pure $ Left $ UnauthorizedForRole "Auteur" subjectType rt [verb']
    where
      hasPerspectiveWithVerb :: RoleType -> RoleType -> MonadPerspectives Boolean
      hasPerspectiveWithVerb subjectType authorizedRoleType = do
        adtOfAuthorizedRoleType <- getRole authorizedRoleType >>= adtOfRole
        isJust <$> findPerspective subjectType
          \perspective -> pure (perspectiveSupportsRoleVerb perspective verb' &&
            unsafePartial (perspective `isPerspectiveOnADT` adtOfAuthorizedRoleType))
