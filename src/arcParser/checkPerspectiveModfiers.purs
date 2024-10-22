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

module Perspectives.Parsing.Arc.PhaseThree.CheckPerspectivesModifiers where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (gets) as State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (execWriterT, tell)
import Data.Array (elemIndex, filter, head, length)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Foreign.Object (values)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Parsing.Arc.PhaseTwoDefs (PhaseThree, withDomeinFile)
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.Role (Role(..), identifierOfRole, kindOfRole, perspectivesOfRole, pos, posOfRole, roleIsFunctional)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.Perspective (Perspective(..), perspectiveSupportsPropertyWithOneofVerbs)
import Perspectives.Representation.TypeIdentifiers (PropertyType(..), RoleKind(..), RoleType(..))
import Perspectives.Representation.Verbs (PropertyVerb(..))
import Perspectives.Types.ObjectGetters (propertiesInPerspective)

checkPerspectiveModifiers :: PhaseThree Unit
checkPerspectiveModifiers = do 
  df@{id} <- lift $ State.gets _.dfr
  -- Take the DomeinFile from PhaseTwoState and temporarily store it in the cache.
  withDomeinFile
    id
    (DomeinFile df)
    do 
      {enumeratedProperties} <- lift $ State.gets _.dfr
      for_ enumeratedProperties checkAuthorOnly
      for_ enumeratedProperties checkSelfOnly

findRolesWithProperty :: PropertyType -> PhaseThree (Array EnumeratedRole)
findRolesWithProperty pt = do
  {enumeratedRoles} <- lift $ State.gets _.dfr
  pure $ filter 
    (\(EnumeratedRole{properties}) -> isJust $ elemIndex pt properties)
    (values enumeratedRoles)

isPerspectiveOnRoleAndProperty :: RoleType -> PropertyType -> Perspective -> MonadPerspectives Boolean
isPerspectiveOnRoleAndProperty role prop p@(Perspective {roleTypes}) = do 
  props <- (propertiesInPerspective p)
  pure ((isJust $ elemIndex prop props) && (isJust $ elemIndex role roleTypes))

findPerspectivesOnProperty :: PropertyType -> PhaseThree (Array {userRole :: Role, objectRole :: EnumeratedRole, perspective :: Perspective} )
findPerspectivesOnProperty prop = do 
  objectRoles <- findRolesWithProperty prop
  userRoles <- userRolesInModel
  lift $ lift $ execWriterT 
    (for_ objectRoles
      (\objectRole -> 
        for_ userRoles
          \userRole -> for_ (perspectivesOfRole userRole)
            \perspective -> (lift $ isPerspectiveOnRoleAndProperty (ENR $ identifier objectRole) prop perspective) >>= if _ 
              then tell [{userRole, objectRole, perspective}]
              else pure unit))

checkAuthorOnly :: EnumeratedProperty -> PhaseThree Unit
checkAuthorOnly (EnumeratedProperty{id:prop, authoronly}) = if authoronly
  then do 
    perspectivesAndRoles <- findPerspectivesOnProperty (ENP prop)
    case head perspectivesAndRoles of 
      (Just {userRole, objectRole, perspective}) -> if ((length perspectivesAndRoles) == 1)
        then do 
          isFunctional <- lift $ lift $ roleIsFunctional userRole
          if isFunctional
            then throwError [PerspectiveCannotBeAuthorOnly (identifierOfRole userRole) (posOfRole userRole) (identifier objectRole) (pos objectRole) prop]
            else pure unit
        else pure unit
      -- Give a warning here?
      Nothing -> pure unit
  else pure unit

userRolesInModel :: PhaseThree (Array Role)
userRolesInModel = do 
  {enumeratedRoles, calculatedRoles} <- lift $ State.gets _.dfr
  pure $ (E <$> filter (eq UserRole <<< kindOfRole) (values enumeratedRoles)) <> 
    (C <$> (filter (eq UserRole <<< kindOfRole) (values calculatedRoles)))

checkSelfOnly  :: EnumeratedProperty -> PhaseThree Unit
checkSelfOnly (EnumeratedProperty{id:prop, selfonly}) = if selfonly
  then do
    perspectivesAndRoles <- findPerspectivesOnProperty (ENP prop)
    perspectivesThatWrite <- pure $ filter 
      (\{perspective} -> perspectiveSupportsPropertyWithOneofVerbs perspective (ENP prop) (PSet [SetPropertyValue, AddPropertyValue, RemovePropertyValue]))
      perspectivesAndRoles 
    if ((length perspectivesAndRoles) == 1)
      then unsafePartial case head perspectivesAndRoles of
        (Just {userRole, objectRole, perspective}) -> throwError [SelfOnlyNeedsTwoRoles (identifierOfRole userRole) (posOfRole userRole) prop]
      else void $ for_ perspectivesAndRoles (\{userRole, objectRole, perspective} -> 
        if (unwrap perspective).isSelfPerspective
          then do 
            isFunctional <- lift $ lift $ roleIsFunctional userRole
            isWritingPerspective <- pure $ perspectiveSupportsPropertyWithOneofVerbs perspective (ENP prop) (PSet [SetPropertyValue, AddPropertyValue, RemovePropertyValue])
            anotherPerspectiveWrites <- if isWritingPerspective
              then pure $ length perspectivesThatWrite > 1
              else pure $ length perspectivesThatWrite > 0
            if isFunctional || not anotherPerspectiveWrites
              then throwError $ [SelfOnlyShouldBeAuthorOnly (identifierOfRole userRole) (posOfRole userRole) prop]
              else pure unit
          else pure unit)
  else pure unit
