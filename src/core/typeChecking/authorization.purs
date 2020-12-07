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
import Data.Array (elemIndex)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Effect.Class (liftEffect)
import Perspectives.CoreTypes (MonadPerspectives, (##>>), (###=), (###>>))
import Perspectives.ErrorLogging (logPerspectivesError)
import Perspectives.Instances.ObjectGetters (roleType, typeOfSubjectOfAction)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.Action (Action(..), Verb)
import Perspectives.Representation.Class.PersistentType (getAction)
import Perspectives.Representation.Class.Role (allProperties, typeIncludingAspectsBinding)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ActionType, EnumeratedPropertyType, EnumeratedRoleType(..), PropertyType(..), RoleType(..))
import Perspectives.Types.ObjectGetters (actionsOfRole_, hasAspect, propertiesOfView, roleTypeAspectsClosure)
import Perspectives.TypesForDeltas (SubjectOfAction)

type Found a = StateT Boolean MonadPerspectives a

-- | True iff the user role represented by the SubjectOfAction argument has a perspective
-- | on the role type that includes an Action
-- |  * with a view that holds the property,
-- |  * and the given Verb.
roleHasPerspectiveOnPropertyWithVerb :: SubjectOfAction -> RoleInstance -> EnumeratedPropertyType -> Verb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnPropertyWithVerb subject roleInstance property verb' = do
  (subjectType :: RoleType) <- typeOfSubjectOfAction subject
  roleType' <- roleInstance ##>> roleType
  (execStateT (run subjectType roleType') false) >>=
    if _
      then pure $ Right true
      else pure $ Left $ UnauthorizedForProperty "Auteur" subjectType roleType' property verb'
  where
    run :: RoleType -> EnumeratedRoleType -> Found Unit
    run subjectType roleType = do
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
                  (Action{verb, requiredObjectProperties}) <- lift $ getAction at
                  if verb == verb'
                    then case requiredObjectProperties of
                      Just vt -> do
                        props <- lift (vt ###= propertiesOfView)
                        case elemIndex (ENP property) props of
                          Nothing -> pure unit
                          otherwise -> put true
                      Nothing -> do
                        props <- lift (allProperties (ST roleType))
                        case elemIndex (ENP property) props of
                          Nothing -> pure unit
                          otherwise -> put true
                    else pure unit

hasBeenFound :: Found Boolean
hasBeenFound = get

-- | True if the user role represented by the SubjectOfAction argument has a perspective
-- | on the role type that includes an Action with the given Verb,
-- | OR if the role type has the aspect "sys:RootContext$RootUser"
roleHasPerspectiveOnRoleWithVerb :: SubjectOfAction -> EnumeratedRoleType -> Array Verb -> MonadPerspectives (Either PerspectivesError Boolean)
roleHasPerspectiveOnRoleWithVerb subject roleType verbs = (roleType ###>> hasAspect (EnumeratedRoleType "model:System$RootContext$RootUser")) >>=
  if _
    then pure $ Right true
    else do
      (subjectType :: RoleType) <- typeOfSubjectOfAction subject
      (execStateT (hasPerspectiveWithVerb subjectType) false) >>=
        if _
          then pure $ Right true
          else pure $ Left $ UnauthorizedForRole "Auteur" subjectType (ENR roleType) verbs
  where
    hasPerspectiveWithVerb :: RoleType -> Found Unit
    hasPerspectiveWithVerb subjectType = do
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
                  (Action{verb, object}) <- lift $ getAction at
                  if (isJust $ elemIndex verb verbs) && object == ENR roleType
                    then put true
                    else pure unit

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
      hasPerspectiveWithVerb subjectType roleType = do
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
                    (Action{verb, object}) <- lift $ getAction at
                    if verb == verb'
                      then case object of
                        CR _ -> if object == roleType then put true else pure unit
                        ENR etype -> do
                          -- By reducing the ADT that represents both the role and its binding, and, when the object is
                          -- calculated, the end result of the calculation, we cover both bound contexts and unbound contexts.
                          (adt :: ADT EnumeratedRoleType) <- lift $ typeIncludingAspectsBinding object
                          if unwrap (reduce ((\rt -> pure (rt == etype)) :: EnumeratedRoleType -> Identity Boolean) adt)
                            then put true
                            else pure unit
                        else pure unit
