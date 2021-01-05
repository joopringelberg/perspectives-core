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

module Perspectives.Checking.Authorization where

import Prelude

import Control.Monad.State (StateT, execStateT, get, lift, put)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (MonadPerspectives, (##>>), (###=), (###>>))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Instances.ObjectGetters (roleType, typeOfSubjectOfAction)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Action (Action(..), Verb)
import Perspectives.Representation.Class.Action (providesPerspectiveOnRole)
import Perspectives.Representation.Class.PersistentType (getAction)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedPropertyType, EnumeratedRoleType, RoleType(..))
import Perspectives.Types.ObjectGetters (actionsOfRole_, hasPerspectiveOnPropertyWithVerb, hasPerspectiveOnRoleWithVerbs, roleTypeAspectsClosure)
import Perspectives.TypesForDeltas (SubjectOfAction)

type Found a = StateT Boolean MonadPerspectives a

hasBeenFound :: Found Boolean
hasBeenFound = get

-- | True iff the user role represented by the SubjectOfAction argument has a perspective
-- | on the role type that includes an Action
-- |  * with a view that holds the property,
-- |  * and the given Verb.
roleHasPerspectiveOnPropertyWithVerb :: SubjectOfAction -> RoleInstance -> EnumeratedPropertyType -> Verb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnPropertyWithVerb subject roleInstance property verb' = do
  (subjectType :: RoleType) <- typeOfSubjectOfAction subject
  roleType' <- roleInstance ##>> roleType
  execStateT (hasPerspectiveOnPropertyWithVerb subjectType property [verb']) false >>=
    if _
      then pure $ Right true
      else pure $ Left $ UnauthorizedForProperty "Auteur" subjectType roleType' property verb'

-- | True if the user role represented by the SubjectOfAction argument has a perspective
-- | on the role type that includes an Action with the given Verb,
-- | OR if the role type has the aspect "sys:RootContext$RootUser"
roleHasPerspectiveOnRoleWithVerb :: SubjectOfAction -> EnumeratedRoleType -> Array Verb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnRoleWithVerb subject roleType verbs = do
  (subjectType :: RoleType) <- typeOfSubjectOfAction subject
  hasPerspective <- roleType ###>> hasPerspectiveOnRoleWithVerbs verbs subjectType
  if hasPerspective
    then pure $ Right true
    else pure $ Left $ UnauthorizedForRole "Auteur" subjectType (ENR roleType) verbs

-- | This function differs from `roleHasPerspectiveOnRoleWithVerb` in that it uses an
-- | `authorizedRole` (second parameter). This is the role that binds the external role that we scrutinize,
-- | or it is the Calculated role that results in external roles.
roleHasPerspectiveOnExternalRoleWithVerb :: SubjectOfAction -> Maybe RoleType -> Verb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnExternalRoleWithVerb subject mroleType verb' = case mroleType of
  Nothing -> do
    (subjectType :: RoleType) <- typeOfSubjectOfAction subject
    liftEffect $ logPerspectivesError $ Custom "roleHasPerspectiveOnExternalRoleWithVerb: no authorizedRole provided to construct external role"
    pure $ Left $ Custom "roleHasPerspectiveOnExternalRoleWithVerb: no authorizedRole provided to construct external role"
  Just rt -> do
    (subjectType :: RoleType) <- typeOfSubjectOfAction subject
    (execStateT (hasPerspectiveWithVerb subjectType rt) false) >>=
      if _
        then pure $ Right true
        else pure $ Left $ UnauthorizedForRole "Auteur" subjectType rt [verb']
    where
      hasPerspectiveWithVerb :: RoleType -> RoleType -> Found Unit
      hasPerspectiveWithVerb subjectType authorizedRoleType = do
        allSubjects <- lift (subjectType ###= roleTypeAspectsClosure)
        for_ allSubjects
          \userRole -> hasBeenFound >>= if _
            then pure unit
            else do
              (as :: Array ActionType) <- lift (userRole ###= actionsOfRole_)
              for_ as
                \at -> hasBeenFound >>= if _
                  then pure unit
                  else do
                    (act@Action{verb}) <- lift $ getAction at
                    if verb == verb'
                      -- This also covers the case that authorizedRoleType is an EnumeratedRoleType.
                      -- In that case, we really want to check whether the Action provides access to the *binding*
                      -- of that EnumeratedRoleType.
                      -- `providesPerspectiveOnRole` works by checking that all EnumeratedRoleTypes in the ADT
                      -- **of its argument and its binding** are in the EnumeratedRoleTypes of the object of the
                      -- action.
                      then (lift $ providesPerspectiveOnRole authorizedRoleType act) >>= put
                      else pure unit
