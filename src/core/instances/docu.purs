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

-- | A Perspectives 'program' consists of a model and some screens. Using these, the end user creates, consults, changes and deletes any number of **instances** of contexts and roles.
-- | * [PerspectContext](Perspectives.InstanceRepresentation.html#t:PerspectContext)
-- | * [PerspectRol](Perspectives.InstanceRepresentation.html#t:PerspectRol)
-- | * like types, instances are [identified](Perspectives.Representation.InstanceIdentifiers#t:x).
-- |
-- | A lot of the functionality of a Perspectives program boils down to traversing the rich structures of Context- and Role instances. The modeller can write down **queries** to traverse paths through the graph. The programmer of the core needs to traverse the same structures. He can use a number of functions to create queries in code:
-- | * [ObjectGetters](Perspectives.Instances.ObjectGetters.html#t:x) are functions that asynchronously retrieve instance representations from the database and traverse from one to another.
-- | * [Combinators](Perspectives.Instances.Combinators.html#t:x) are higher order functions to combine basic ObjectGetters.
-- | * The [ArrayTransformer](Perspectives.DependencyTracking.Array.Trans.html#t:x) provides an abstraction to deal with sequences of values (a role can have multiple instances in a context: just think of the Guest role at a Party).



module Perspectives.Docu.Instances where
