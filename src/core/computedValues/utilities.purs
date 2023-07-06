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

-- | This module defines External Core functions for model://perspectives.domains#Couchdb.

module Perspectives.Extern.Utilities where

import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, singleton)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..))
import Data.String (replace) as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Perspectives.CoreTypes (MonadPerspectivesQuery)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Instances.Values (parseInt)
import Perspectives.Persistence.State (getSystemIdentifier)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance(..), RoleInstance(..), Value(..))
import Prelude (pure, show, ($), (<<<), (>>=), bind)
import Unsafe.Coerce (unsafeCoerce)

-- TODO: verander naar echte gegenereerde identifiers.
genSym :: RoleInstance -> MonadPerspectivesQuery String
genSym _ = pure "geheim"

-- | Returns a random number in the closed interval [l;u].
random :: Array String -> Array String -> RoleInstance -> MonadPerspectivesQuery Value
random lower_ upper_ _ = ArrayT case head lower_, head upper_ of
  Just l, Just u -> do 
    lowerBound <- try $ liftAff $ parseInt l
    upperBound <- try $ liftAff $ parseInt u
    case lowerBound, upperBound of 
      Right lower, Right upper -> (liftEffect (randomInt lower upper)) >>= pure <<< singleton <<< Value <<< show
      _, _ -> pure []
  _, _ -> pure []

roleIdentifier :: RoleInstance -> MonadPerspectivesQuery String
roleIdentifier (RoleInstance id) = pure id

contextIdentifier :: ContextInstance -> MonadPerspectivesQuery String
contextIdentifier (ContextInstance id) = pure id

systemIdentifier :: RoleInstance -> MonadPerspectivesQuery String
systemIdentifier _ = lift $ lift getSystemIdentifier

replace :: Array String -> Array String -> String -> MonadPerspectivesQuery String
replace patterns replacements value = case head patterns, head replacements of
  Just pattern, Just replacement -> pure $ String.replace (Pattern pattern) (Replacement replacement) value
  _, _ -> pure value

-- | An Array of External functions. Each External function is inserted into the ExternalFunctionCache and can be retrieved
-- | with `Perspectives.External.HiddenFunctionCache.lookupHiddenFunction`.
externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#Utilities$GenSym" {func: unsafeCoerce genSym, nArgs: 0}
  , Tuple "model://perspectives.domains#Utilities$RoleIdentifier" {func: unsafeCoerce roleIdentifier, nArgs: 0}
  , Tuple "model://perspectives.domains#Utilities$ContextIdentifier" {func: unsafeCoerce contextIdentifier, nArgs: 0}
  , Tuple "model://perspectives.domains#Utilities$SystemIdentifier" {func: unsafeCoerce systemIdentifier, nArgs: 0}
  , Tuple "model://perspectives.domains#Utilities$Replace" {func: unsafeCoerce replace, nArgs: 2}
  , Tuple "model://perspectives.domains#Utilities$Random" {func: unsafeCoerce random, nArgs: 2}
  ]
