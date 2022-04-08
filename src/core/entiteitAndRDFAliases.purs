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

module Perspectives.EntiteitAndRDFAliases where

{-
When talking about Deltas or Triples, we use these terms:
  <Subject, Predicate, Object(s)>
To be precise: a Delta has a (nullOrUndefined Object), while a Triple is modelled as an (Array Object) - where Object is an alias for String.
All three positions can be occupied with identifiers of Resources (in terms of RDF an URI), though the Object position can be filled with a value-expression, too (in terms of RDF a Literal)

When talking about Contexts and Roles, we use the terms:
  <ContextID, RolName, RolID>
  <RolID, PropertyName, Value>

All six positions are occupied by identifiers of contexts and roles (in CRL), except for the Value position that is filled with the String representation of simple values (Boolean, String, Number, Date).

If we generalize over Context and Role, we use the following terms:
  - ID for ContextID and RolID
  - MemberName for RolName and PropertyName
  - Value for RolID and Value.
-}
-----------------------------------------------------------
-- RDF
-----------------------------------------------------------

type Subject = String
type Predicate = String
type Object = String
type Objects = Array String

-----------------------------------------------------------
-- CONTEXT AND ROL
-----------------------------------------------------------
type ID = String
type ContextID = String
type RolID = String
type PropertyName = String
type RolName = String
type MemberName = String
type Comment = String
type Value = String
type Values = Array String

-----------------------------------------------------------
-- VIEW
-----------------------------------------------------------
type ViewName = String
