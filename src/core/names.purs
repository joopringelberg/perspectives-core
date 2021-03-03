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
import Control.Monad.AvarMonadAsk (gets)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable, lookup) as OBJ
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.Identifiers (deconstructLocalNameFromCurie, deconstructPrefix, isQualifiedWithDomein)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Persistence.API (getSystemIdentifier)
import Prelude (append, bind, flip, pure, ($), (<<<), (<>), (>>=))

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

-- | Replace model:User$Me by "model:User$<guid>$User_0001".
expandIndexedNames :: OBJ.Object String -> String -> String
expandIndexedNames defaults expandedName =
  case OBJ.lookup expandedName defaults of
    (Just ind) -> ind
    Nothing -> expandedName

expandNamespaces :: OBJ.Object String -> String -> String
expandNamespaces namespaces s = if isQualifiedWithDomein s then s else
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
  [ Tuple "cdb" "model:Couchdb"
  , Tuple "sys" "model:System"
  , Tuple "usr" "model:User"
  , Tuple "ser" "model:Serialise"
  , Tuple "p" "model:Parsing"
  ]

defaultIndexedNames :: MonadPerspectives (OBJ.Object String)
defaultIndexedNames = do
  user <- getUserIdentifier
  pure $ OBJ.fromFoldable
    [ Tuple "model:User$Me" user
    ]

-----------------------------------------------------------
-- LOOKUP INDEXED NAMES
-----------------------------------------------------------
-- | Look up a fully qualified indexed name, e.g. model:User$Me
lookupIndexedRole :: String -> MonadPerspectives (Maybe RoleInstance)
lookupIndexedRole iname = gets _.indexedRoles >>= pure <<< OBJ.lookup iname

-- | Look up a fully qualified indexed name, e.g. model:User$MySystem
lookupIndexedContext :: String -> MonadPerspectives (Maybe ContextInstance)
lookupIndexedContext iname = gets _.indexedContexts >>= pure <<< OBJ.lookup iname

-----------------------------------------------------------
-- CONVENIENCE NAMESPACE PREFIX FUNCIONS
-----------------------------------------------------------
q :: String -> String
q ln = "model:QueryAst$" <> ln

psp :: String -> String
psp ln = "model:Perspectives$" <> ln

-----------------------------------------------------------
-- SYSTEM AND USER
-----------------------------------------------------------
-- | Returns a Perspectives Identifier of the form "model:User$<systemIdentifier>$User".
getUserIdentifier :: MonadPerspectives String
getUserIdentifier = getMySystem >>= pure <<< flip append "$User"
-- getUserIdentifier = do
--   me <- lookupIndexedRole "model:System$Me"
--   case me of
--     Nothing -> throwError (error "Indexed name 'model:System$Me' should be available!")
--     Just (RoleInstance m) -> pure m

-- | Returns a Perspectives Identifier of the form "model:User$<guid>"
getMySystem :: MonadPerspectives String
getMySystem = getSystemIdentifier >>= pure <<< append "model:User$"
