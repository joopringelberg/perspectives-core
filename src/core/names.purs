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

module Perspectives.Names

where

import Control.Monad.AvarMonadAsk (gets, modify)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, delete, filter, fromFoldable, keys, lookup) as OBJ
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (deconstructLocalNameFromCurie, deconstructPrefix, isTypeUri)
import Perspectives.ModelDependencies (sysMe)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Prelude (Unit, append, bind, eq, flip, pure, ($), (<<<), (<>), (>>=))

-----------------------------------------------------------
-- EXPAND DEFAULT NAMESPACES
-----------------------------------------------------------
-- | Useful for expanding local names used in bindings, property- and view references.
expandDefaultNamespaces :: String -> MonadPerspectives String
expandDefaultNamespaces n = do
  names <- defaultIndexedNames
  (pure $ expandNamespaces defaultNamespaces n) >>= pure <<< expandIndexedNames names

-- | As expandDefaultNamespaces, but provide both indexed names and namespaces.
expandDefaultNamespaces_ :: OBJ.Object String -> OBJ.Object String -> String -> String
expandDefaultNamespaces_ indexedNames namespaces n = expandIndexedNames indexedNames (expandNamespaces namespaces n)

-- | Replace model:System$Me by "def:#<guid>$User".
expandIndexedNames :: OBJ.Object String -> String -> String
expandIndexedNames defaults expandedName =
  case OBJ.lookup expandedName defaults of
    (Just ind) -> ind
    Nothing -> expandedName

expandNamespaces :: OBJ.Object String -> String -> String
expandNamespaces namespaces s = if isTypeUri s then s else
  case deconstructPrefix s of
    (Just pre) -> do
      case OBJ.lookup pre namespaces of
        (Just modelName) -> case deconstructLocalNameFromCurie s of
          (Just ln) -> (modelName <> "$" <> ln )
          Nothing -> s
        Nothing -> s
    Nothing -> s

defaultNamespaces :: OBJ.Object String
defaultNamespaces = OBJ.fromFoldable
  [ Tuple "sys" "model://perspectives.domains#System"
  , Tuple "cm" "model://perspectives.domains#CouchdbManagement"
  -- External core modules: this depends on the list in module Perspectives.External.CoreModules.
  , Tuple "cdb" "model://perspectives.domains#Couchdb"
  , Tuple "ser" "model://perspectives.domains#Serialise"
  , Tuple "parse" "model://perspectives.domains#Parsing"
  , Tuple "util" "model://perspectives.domains#Utilities"
  , Tuple "sens" "model://perspectives.domains#Sensor"
  , Tuple "rabbit" "model://perspectives.domains#RabbitMQ"
  , Tuple "files" "model://perspectives.domains#Files"
  , Tuple "bs" "model://perspectives.domains#BrokerServices"
  ]

defaultIndexedNames :: MonadPerspectives (OBJ.Object String)
defaultIndexedNames = do
  user <- getUserIdentifier
  pure $ OBJ.fromFoldable
    [ Tuple sysMe user
    ]

-----------------------------------------------------------
-- LOOKUP INDEXED NAMES
-----------------------------------------------------------
-- | Look up a fully qualified indexed name, e.g. model:System$Me
lookupIndexedRole :: String -> MonadPerspectives (Maybe RoleInstance)
lookupIndexedRole iname = gets _.indexedRoles >>= pure <<< OBJ.lookup iname

-- | Look up a fully qualified indexed name, e.g. maps "sys:Me" to "def:#<GUID>$User".
lookupIndexedContext :: String -> MonadPerspectives (Maybe ContextInstance)
lookupIndexedContext iname = gets _.indexedContexts >>= pure <<< OBJ.lookup iname

-----------------------------------------------------------
-- REVERSE LOOKUP RESOURCES IN INDEXED NAMES
-----------------------------------------------------------
-- | If the role instance is in fact an indexed resource, returns the identifier (key) under which it is stored in state.
findIndexedRoleName :: RoleInstance -> MonadPerspectives (Maybe String)
findIndexedRoleName rid = gets _.indexedRoles >>= pure <<< head <<< OBJ.keys <<< OBJ.filter (eq rid)

findIndexedContextName :: ContextInstance -> MonadPerspectives (Maybe String)
findIndexedContextName cid = gets _.indexedContexts >>= pure <<< head <<< OBJ.keys <<< OBJ.filter (eq cid)

-----------------------------------------------------------
-- REMOVE INDEXED NAMES
-----------------------------------------------------------
removeIndexedRole :: String -> MonadPerspectives Unit
removeIndexedRole s = modify \r -> r {indexedRoles = OBJ.delete s r.indexedRoles}

removeIndexedContext :: String -> MonadPerspectives Unit
removeIndexedContext s = modify \r -> r {indexedContexts = OBJ.delete s r.indexedContexts}

-----------------------------------------------------------
-- SYSTEM AND USER
-----------------------------------------------------------
-- | Returns a Perspectives Identifier of the form "def:#<systemIdentifier>$User".
getUserIdentifier :: MonadPerspectives String
getUserIdentifier = getMySystem >>= pure <<< flip append "$User"

-- | Returns a Perspectives Identifier of the form "def:#<guid>"
-- | To be more precise: "def:#<SystemIdentifier>"
getMySystem :: MonadPerspectives String
getMySystem = getSystemIdentifier >>= \sysId -> pure $ "def:#" <> sysId

