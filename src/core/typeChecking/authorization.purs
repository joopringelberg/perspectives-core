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
import Perspectives.Instances.ObjectGetters (roleType)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Class.Role (adtOfRole, getRole)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.Perspective (perspectiveSupportsOneOfRoleVerbs)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, PropertyType(..), RoleType(..))
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerb)
import Perspectives.Types.ObjectGetters (findPerspective, hasPerspectiveOnPropertyWithVerb, hasPerspectiveOnRoleWithVerbs, isPerspectiveOnADT)

type Found a = StateT Boolean MonadPerspectives a

hasBeenFound :: Found Boolean
hasBeenFound = get

-- | True iff the user role represented by the subject argument has a perspective
-- | on the RoleType (in any state) with PropertyVerbs
-- |  * whose properties include the property,
-- |  * whose verbs include the given Verb.
roleHasPerspectiveOnPropertyWithVerb :: RoleType -> RoleInstance -> EnumeratedPropertyType -> PropertyVerb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnPropertyWithVerb subject roleInstance property verb' = do
  roleType' <- roleInstance ##>> roleType
  (unsafePartial $ hasPerspectiveOnPropertyWithVerb subject roleType' property verb') >>=
    if _
      then pure $ Right true
      else pure $ Left $ UnauthorizedForProperty "Auteur" subject (ENR roleType') (ENP property) verb' Nothing Nothing

-- | True if the user role represented by the subject argument has a perspective
-- | on the RoleType that includes an Action with the given Verb,
-- | OR if the subject has the aspect "sys:RootContext$RootUser"
roleHasPerspectiveOnRoleWithVerb :: RoleType -> EnumeratedRoleType -> Array RoleVerb -> Maybe ArcPosition -> Maybe ArcPosition -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnRoleWithVerb subject roleType verbs mstart mend = do
  hasPerspective <- unsafePartial (roleType ###>> hasPerspectiveOnRoleWithVerbs verbs subject)
  if hasPerspective
    then pure $ Right true
    else pure $ Left $ UnauthorizedForRole "Auteur" subject (ENR roleType) verbs mstart mend

-- | This function differs from `roleHasPerspectiveOnRoleWithVerb` in that it uses an
-- | `authorizedRole` (second parameter). This is the role that binds the external role that we scrutinize,
-- | or it is the Calculated role that results in external roles. It is this contextrole (or calculated role)
-- | that the subject is authorized for.
roleHasPerspectiveOnExternalRoleWithVerbs :: RoleType -> Maybe RoleType -> Array RoleVerb -> Maybe ArcPosition -> Maybe ArcPosition -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnExternalRoleWithVerbs subject mroleType verbs mstart mend = case mroleType of
  Nothing -> do
    liftEffect $ logPerspectivesError $ Custom "roleHasPerspectiveOnExternalRoleWithVerb: no authorizedRole provided to construct external role"
    pure $ Left $ Custom "roleHasPerspectiveOnExternalRoleWithVerb: no authorizedRole provided to construct external role"
  Just rt -> do
    (hasPerspectiveWithVerb subject rt) >>=
      if _
        then pure $ Right true
        else pure $ Left $ UnauthorizedForRole "Auteur" subject rt verbs mstart mend
    where
      hasPerspectiveWithVerb :: RoleType -> RoleType -> MonadPerspectives Boolean
      hasPerspectiveWithVerb subjectType authorizedRoleType = do
        adtOfAuthorizedRoleType <- getRole authorizedRoleType >>= adtOfRole
        isJust <$> findPerspective subjectType
          \perspective -> do
            s <- pure $ perspectiveSupportsOneOfRoleVerbs perspective verbs
            p <- unsafePartial (perspective `isPerspectiveOnADT` adtOfAuthorizedRoleType)
            pure (s && p)
