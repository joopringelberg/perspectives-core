-- BEGIN LICENSE
-- Perspectives Distributed Runtime
-- SPDX-FileCopyrightText: 2024 Joop Ringelberg (joopringelberg@perspect.it), Cor Baars
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

module Perspectives.InvertedQueryKey  where

import Prelude

import Data.Newtype (unwrap)
import Foreign (ForeignError(..), fail, F)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType, EnumeratedRoleType)
import Simple.JSON (class ReadForeign, class WriteForeign, read', write)

-----------------------------------------------------------
-- RUNTIME KEYS FOR INVERTED QUERIES
-- Runtime keys identify exactly one path through instance space.
-----------------------------------------------------------
type PropertyKeyFields = 
  { property :: EnumeratedPropertyType
  , role :: EnumeratedRoleType}

type RoleKeyFields = 
  { context_origin :: ContextType
  , role_destination :: EnumeratedRoleType}
type ContextKeyFields =
  { role_origin :: EnumeratedRoleType
  , context_destination :: ContextType}

type FillerKeyFields = 
  { filledRole_origin :: EnumeratedRoleType
  , filledContext_origin :: ContextType
  , fillerRole_destination :: EnumeratedRoleType
  , fillerContext_destination :: ContextType}

type FilledKeyFields = 
  { fillerRole_origin :: EnumeratedRoleType
  , fillerContext_origin :: ContextType
  , filledRole_destination :: EnumeratedRoleType
  , filledContext_destination :: ContextType}

data RunTimeInvertedQueryKey = 
    RTPropertyKey PropertyKeyFields
  | RTRoleKey RoleKeyFields
  | RTContextKey ContextKeyFields
  -- The filler step takes us from a filled role to its filler.
  | RTFillerKey FillerKeyFields
  -- The filled step takes us from a filler to the role that it fills.
  -- Each combination of an element in fillerRoleInContexts with filledRoleInContext is a valid runtime key.
  | RTFilledKey FilledKeyFields

instance WriteForeign RunTimeInvertedQueryKey where
  writeImpl (RTPropertyKey r) = write { keyType: "RTPropertyKey", fields: r}
  writeImpl (RTRoleKey r) = write { keyType: "RTRoleKey", fields: r}
  writeImpl (RTContextKey r) = write { keyType: "RTContextKey", fields: r}
  writeImpl (RTFillerKey r) = write { keyType: "RTFillerKey", fields: r}
  writeImpl (RTFilledKey r) = write { keyType: "RTFilledKey", fields: r}  

instance ReadForeign RunTimeInvertedQueryKey where
  readImpl f = do
    {keyType} :: {keyType :: String} <- read' f
    case keyType of 
      "RTPropertyKey" -> RTPropertyKey <<< _.fields <$> ((read' f) :: F {fields :: PropertyKeyFields})
      "RTRoleKey" -> RTRoleKey <<< _.fields <$> ((read' f) :: F {fields :: RoleKeyFields})
      "RTContextKey" -> RTContextKey <<< _.fields <$> ((read' f) :: F {fields :: ContextKeyFields})
      "RTFillerKey" -> RTFillerKey <<< _.fields <$> ((read' f) :: F {fields :: FillerKeyFields})
      "RTFilledKey" -> RTFilledKey <<< _.fields <$> ((read' f) :: F {fields :: FilledKeyFields})
      key -> fail $ ForeignError $ "Unknown constructor for InvertedQueryKey: " <> key

instance Eq RunTimeInvertedQueryKey where
  eq (RTPropertyKey r1) (RTPropertyKey r2) = eq r1 r2
  eq (RTRoleKey r1) (RTRoleKey r2) = eq r1 r2
  eq (RTContextKey r1) (RTContextKey r2) = eq r1 r2
  eq (RTFillerKey r1) (RTFillerKey r2) = eq r1 r2
  eq (RTFilledKey r1) (RTFilledKey r2) = eq r1 r2
  eq _ _ = false

instance Show RunTimeInvertedQueryKey where
  show (RTPropertyKey r) = "RTPropertyKey " <> show r
  show (RTRoleKey r) = "RTRoleKey " <> show r
  show (RTContextKey r) = "RTContextKey" <> show r
  show (RTFillerKey r) = "RTFillerKey" <> show r
  show (RTFilledKey r) = "RTFilledKey" <> show r

-- | Construct a string out of a RuntimeInvertedQueryKey.
-- | Use these keys for the Couchdb views.
-- | Note that the keys must be unique PER VIEW, not in an absolute sense. 
serializeInvertedQueryKey :: RunTimeInvertedQueryKey -> String
serializeInvertedQueryKey (RTPropertyKey {property, role}) = unwrap property <> unwrap role
serializeInvertedQueryKey (RTRoleKey {context_origin, role_destination}) = 
  unwrap context_origin <> unwrap role_destination
serializeInvertedQueryKey (RTContextKey {role_origin, context_destination}) = 
  unwrap role_origin <> unwrap context_destination
serializeInvertedQueryKey (RTFillerKey {filledRole_origin, filledContext_origin, fillerRole_destination, fillerContext_destination}) = 
  unwrap filledRole_origin <> unwrap filledContext_origin <> unwrap fillerRole_destination <> unwrap fillerContext_destination
serializeInvertedQueryKey (RTFilledKey {fillerRole_origin, fillerContext_origin, filledRole_destination, filledContext_destination}) = 
  unwrap fillerRole_origin <> unwrap fillerContext_origin <> unwrap filledRole_destination <> unwrap filledContext_destination