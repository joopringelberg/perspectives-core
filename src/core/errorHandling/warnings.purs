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

module Perspectives.Warning where

import Prelude

import Data.Foldable (intercalate)
import Data.Newtype (unwrap)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType, RoleType, StateIdentifier, roletype2string)

data PerspectivesWarning =
    ModelLacksModelId String
  | ModelLacksUrl String
  | PropertySynchronizationIncomplete EnumeratedPropertyType RoleType (Array RoleType)
  | RoleSynchronizationIncomplete EnumeratedRoleType RoleType (Array RoleType)
  | RoleBindingSynchronizationIncomplete EnumeratedRoleType RoleType (Array RoleType)
  | NotificationError StateIdentifier
  | AutomaticActionError StateIdentifier
  | ExternalFunctionError String String
  | NoTranslations String

instance showPerspectivesWarning :: Show PerspectivesWarning where
  show (ModelLacksModelId dfid) = "(ModelLacksModelId) The model '" <> dfid <> "' lacks a value for the property ModelIdentification on its Model instance."
  show (ModelLacksUrl dfid) = "(ModelLacksUrl) The model '" <> dfid <> "' lacks a value for the property Url on its Model instance."
  show (PropertySynchronizationIncomplete prop source destinations) = "(PropertySynchronizationIncomplete) Modifications to property:\n\t'" <> (unwrap prop) <> "'\n by:\n\t'" <> roletype2string source <> "'\n cannot be sent to:\n\t* " <> intercalate "\n\t* " (map roletype2string destinations) <> "."
  show (RoleSynchronizationIncomplete role source destinations) = "(RoleSynchronizationIncomplete) New (and removed) instances of role type:\n\t'" <> (unwrap role) <> "'\n by:\n\t'" <> roletype2string source <> "'\n cannot be sent to:\n\t* " <> intercalate "\n\t* " (map roletype2string destinations) <> "."
  show (RoleBindingSynchronizationIncomplete role source destinations) = "(RoleBindingSynchronizationIncomplete) Filling (and emptying) instances of role type:\n\t'" <> (unwrap role) <> "'\n by:\n\t'" <> roletype2string source <> "'\n cannot be communicated with:\n\t* " <> intercalate "\n\t* " (map roletype2string destinations) <> "."
  show (NotificationError stateId) = "(NotificationError) Error on notifying in state " <> unwrap stateId
  show (AutomaticActionError stateId) = "(AutomaticActionError) Error on executing automatic action in state " <> unwrap stateId 
  show (ExternalFunctionError fname errorstring) = "(ExternalFunctionError) External library function '" <> fname <> "' results in an error: " <> errorstring
  show (NoTranslations domain) = "(NoTranslations) No translations found for domain '" <> domain <> "'."