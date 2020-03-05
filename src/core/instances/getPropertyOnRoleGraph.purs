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

module Perspectives.Instances.GetPropertyOnRoleGraph where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Array (elemIndex, uncons)
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MP, type (~~>))
import Perspectives.Instances.ObjectGetters (roleType_, binding) as OG
import Perspectives.Query.Compiler (getPropertyFunction)
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Role (binding, propertiesOfADT, roleAspectsADT)
import Perspectives.Representation.InstanceIdentifiers (RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (EnumeratedRoleType, propertytype2string)

-- From a string that maybe identifies a Property (Enumerated or Calculated), construct a function to get values
-- for that Property from a Role instance. Notice that this function may fail.
getPropertyGetter :: String -> EnumeratedRoleType -> MP (RoleInstance ~~> Value)
getPropertyGetter pt rt = do
  isLocal <- pt `isLocallyRepresentedOn` rt
  if isLocal
    then getPropertyFunction pt
    else do
      g <- (getEnumeratedRole rt >>= binding) >>= dispatchOnBindingType pt
      pure (OG.binding >=> g)

-- | True iff the values of the property are represented on instances of the roletype.
-- | This is the case if the property is in the namespace of the roletype, or in one of its aspects.
isLocallyRepresentedOn :: String -> EnumeratedRoleType -> MP Boolean
isLocallyRepresentedOn pt rt = do
  localProps <- getEnumeratedRole rt >>= roleAspectsADT >>= propertiesOfADT >>= pure <<< map propertytype2string
  pure $ isJust $ elemIndex pt localProps

dispatchOnBindingType :: String -> ADT EnumeratedRoleType -> MP (RoleInstance ~~> Value)
dispatchOnBindingType pt (ST r) = getPropertyGetter pt r
dispatchOnBindingType pt EMPTY = throwError (error ("dispatchOnBindingType: cannot handle EMPTY for " <> pt))
dispatchOnBindingType pt UNIVERSAL = throwError (error ("dispatchOnBindingType: cannot handle UNIVERSAL for " <> pt))
dispatchOnBindingType pt (PROD _) = throwError (error ("dispatchOnBindingType: cannot handle PRODUCT for " <> pt))
dispatchOnBindingType pt (SUM roles) = do
  (dispatchers :: Array Dispatcher) <- unsafePartial $ traverse dispatchOn roles
  pure $ untilOneSucceeds dispatchers
  where
    untilOneSucceeds :: Array Dispatcher -> (RoleInstance ~~> Value)
    untilOneSucceeds dispatchers roleInstance = do
      typeOfInstance <- lift $ lift $ OG.roleType_ roleInstance
      untilOneSucceeds_ typeOfInstance dispatchers roleInstance

    untilOneSucceeds_ :: EnumeratedRoleType -> Array Dispatcher -> (RoleInstance ~~> Value)
    untilOneSucceeds_ typeOfInstance dispatchers roleInstance = case uncons dispatchers of
      -- For now, we fail silently.
      Nothing -> empty
      Just {head:(Tuple guard getter), tail} -> do
        isType <- lift $ lift $ guard typeOfInstance
        if isType
          then getter roleInstance
          else untilOneSucceeds_ typeOfInstance tail roleInstance

    dispatchOn :: Partial => ADT EnumeratedRoleType -> MP Dispatcher
    dispatchOn (ST rt) = do
      getter <- getPropertyGetter pt rt
      guard <- pure (\typeOfInstance -> pure (typeOfInstance == rt))
      pure $ Tuple guard getter

type Dispatcher = Tuple Guard Getter
type Guard = EnumeratedRoleType -> MP Boolean
type Getter = RoleInstance ~~> Value
