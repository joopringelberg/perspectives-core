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

-- | Perspectives is a strongly typed representation language. Define your own types using Contexts, Roles, Views, etc.
-- | These types are represented in several modules.
-- | * [Context](Perspectives.Representation.Context.html#t:ContextClass)
-- | * [EnumeratedRole](Perspectives.Representation.EnumeratedRole.html#t:EnumeratedRole)
-- | * [CalculatedRole](Perspectives.Representation.CalculatedRole.html#t:CalculatedRole)
-- | * [EnumeratedProperty](Perspectives.Representation.EnumeratedProperty.html#t:EnumeratedProperty)
-- | * [CalculatedProperty](Perspectives.Representation.CalculatedProperty.html#t:CalculatedProperty)
-- | * [Action](Perspectives.Representation.Action.html#t:Action)
-- | * [View](Perspectives.Representation.View.html#t:View)
-- |
-- | Types can be combined in [Abstract Data types](Perspectives.Representation.ADT.html#t:ADT)
-- |
-- | Types are complex representations that are identified by Purescript newtypes, see
-- | * [Type identifiers](Perspectives.Representation.TypeIdentifiers.html#t:x)
-- | * the [Identifiable](Perspectives.Representation.Class.Identifiable.html#t:x) module for the relation between a type and its identifier.
-- |
-- | Type representations are persisted to a database in a so-called DomeinFile. See:
-- | * [PersistentType](Perspectives.Representation.Class.PersistentType.html#t:PersistentType)
-- | * [Perspectives.DomeinFile](Perspectives.DomeinFile.html#t:DomeinFile)
-- | * [DomeinCache](Perspectives.DomeinCache.html#t:DomeinCache)
-- |
-- | Several Type Classes provide a layer of abstraction:
-- | * [Role](Perspectives.Representation.Class.Role.html#t:x) over `EnumeratedRole` and `CalculatedRole`
-- | * [Property](Perspectives.Representation.Class.Property.html#t:x) over `EnumeratedProperty` and `CalculatedProperty`
-- | * [Action](Perspectives.Representation.Class.Action.html#t:x)


module Perspectives.Docu.TypeRepresentation where
