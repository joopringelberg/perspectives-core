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

module Perspectives.Representation.Class.Context where

import Data.Array (cons, null)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Parsing.Arc.Position (ArcPosition)
import Perspectives.Representation.ADT (ADT(..), product, reduce)
import Perspectives.Representation.Class.Identifiable (identifier)
import Perspectives.Representation.Class.PersistentType (getContext)
import Perspectives.Representation.Context (Context(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType, RoleType, externalRoleType)
import Prelude ((<<<), (<>), (<$>), ($), bind, pure)

-----------------------------------------------------------
-- CONTEXT TYPE CLASS
-----------------------------------------------------------
class ContextClass c where
  contextAspects :: c -> Array ContextType
  defaultPrototype :: c -> Maybe ContextInstance
  roleInContext :: c -> Array RoleType
  contextRole :: c -> Array RoleType
  externalRole :: c -> EnumeratedRoleType
  userRole :: c -> Array RoleType
  aspects :: c -> Array ContextType
  nestedContexts :: c -> Array ContextType
  position :: c -> ArcPosition
  roles :: c -> Array RoleType
  contextADT :: c -> ADT ContextType
  -- The product of the Context and its direct Aspects.
  contextAspectsADT :: c -> ADT ContextType

instance contextContextClass :: ContextClass Context where
  contextAspects = _.contextAspects <<< unwrap
  defaultPrototype = _.defaultPrototype <<< unwrap
  roleInContext = _.rolInContext <<< unwrap
  contextRole = _.contextRol <<< unwrap
  externalRole (Context{_id}) = externalRoleType _id
  userRole = _.gebruikerRol <<< unwrap
  aspects = _.contextAspects <<< unwrap
  nestedContexts = _.nestedContexts <<< unwrap
  position = _.pos <<< unwrap
  roles r = roleInContext r <> contextRole r <> userRole r
  contextADT = ST <<< _._id <<< unwrap
  contextAspectsADT c@(Context{contextAspects}) = let
    aspects = ST <$> contextAspects in
      if null aspects
        then ST $ identifier c
        else PROD (cons (ST $ identifier c) aspects)

-----------------------------------------------------------
-- ALL NESTED CONTEXT TYPES
-----------------------------------------------------------
-- | All nested context types in the context, computed recursively Aspects, of the Context ADT.
allContextTypes :: ADT ContextType -> MonadPerspectives (Array ContextType)
allContextTypes = reduce magic
  where
    magic :: ContextType -> MonadPerspectives (Array ContextType)
    magic ct = do
      ctxt@(Context{contextAspects, nestedContexts}) <- getContext ct
      if null contextAspects
        then pure $ cons ct nestedContexts
        else do
          types <- reduce magic (product (ST <$> contextAspects))
          pure $ cons ct (nestedContexts <> types)
