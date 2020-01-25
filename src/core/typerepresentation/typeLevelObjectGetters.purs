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

module Perspectives.Types.ObjectGetters where

import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Plus (map, (<|>))
import Data.Array (filter, find, singleton)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect.Exception (error)
import Foreign.Object (keys)
import Perspectives.CoreTypes (MonadPerspectives, type (~~~>), MP)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (areLastSegmentsOf, endsWithSegments)
import Perspectives.Instances.Combinators (closure_, filter')
import Perspectives.Parsing.Messages (PerspectivesError(..))
import Perspectives.Representation.ADT (ADT(..), reduce)
import Perspectives.Representation.Class.PersistentType (getPerspectType, getContext)
import Perspectives.Representation.Class.Role (class RoleClass, properties, propertiesOfADT, roleSet, views, viewsOfADT)
import Perspectives.Representation.Context (Context, aspects, roleInContext, externalRole, contextRole, userRole) as Context
import Perspectives.Representation.Context (contextADT)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.TypeIdentifiers (CalculatedRoleType(..), ContextType(..), EnumeratedRoleType(..), PropertyType, RoleType(..), ViewType, propertytype2string, roletype2string)
import Perspectives.Representation.View (propertyReferences)
import Prelude (bind, pure, show, ($), (<<<), (<>), (==), (>=>), (>>=), (>>>))

externalRoleOfADT_ :: ADT ContextType ~~~> EnumeratedRoleType
externalRoleOfADT_ = ArrayT <<< reduce eRole where
  eRole :: ContextType -> MP (Array EnumeratedRoleType)
  eRole = getContext >=> pure <<< singleton <<< Context.externalRole

externalRoleOfADT :: ADT ContextType -> MP (ADT EnumeratedRoleType)
externalRoleOfADT = reduce eRole where
  eRole :: ContextType -> MP (ADT EnumeratedRoleType)
  eRole = getContext >=> pure <<< ST <<< Context.externalRole

-- | If a role with the given qualified name is available, return it as a RoleType. From the type we can find out its RoleKind, too.
lookForRoleType :: String -> (ContextType ~~~> RoleType)
lookForRoleType s c = (lift $ getContext c) >>= pure <<< contextADT >>= lookForRoleTypeOfADT s

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedRoleType :: String -> ContextType ~~~> RoleType
lookForUnqualifiedRoleType s c = (lift $ getContext c) >>= pure <<< contextADT >>= lookForUnqualifiedRoleTypeOfADT s

lookForRoleTypeOfADT :: String -> (ADT ContextType ~~~> RoleType)
lookForRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> ((==) s)) s

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedRoleTypeOfADT :: String -> ADT ContextType ~~~> RoleType
lookForUnqualifiedRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> areLastSegmentsOf s) s

lookForRoleOfADT :: (RoleType -> Boolean) -> String -> ADT ContextType ~~~> RoleType
lookForRoleOfADT criterium rname adt = ArrayT $ roleSet adt >>= case _ of
  Empty -> pure []
  PSet rs -> case find criterium rs of
    Nothing -> pure []
    Just r -> pure [r]
  Universal -> throwError (error $ show (ContextHasNoRole adt rname))

roleInContext :: ContextType ~~~> RoleType
roleInContext = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< Context.roleInContext)

contextRole :: ContextType ~~~> RoleType
contextRole = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< Context.contextRole)

userRole :: ContextType ~~~> RoleType
userRole = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< map ENR <<< Context.userRole)

contextAspectsClosure :: ContextType ~~~> ContextType
contextAspectsClosure = closure_ f where
  f :: ContextType -> ArrayT MonadPerspectives ContextType
  f = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< Context.aspects)

lookForPropertyType_ :: String -> (EnumeratedRoleType ~~~> PropertyType)
lookForPropertyType_ s i = lookForProperty (propertytype2string >>> ((==) s)) (ST i)

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedPropertyType_ :: String -> (EnumeratedRoleType ~~~> PropertyType)
lookForUnqualifiedPropertyType_ s i = lookForProperty (propertytype2string >>> areLastSegmentsOf s) (ST i)

-- | Look for a Property on a given ADT, using a qualified name.
-- | Note: use this function to check that the property is actually defined.
lookForPropertyType :: String -> (ADT EnumeratedRoleType ~~~> PropertyType)
lookForPropertyType s = lookForProperty (propertytype2string >>> ((==) s))

-- | Look for a Property on a given ADT, using the postfix of a name.
lookForUnqualifiedPropertyType :: String -> (ADT EnumeratedRoleType ~~~> PropertyType)
lookForUnqualifiedPropertyType s = lookForProperty (propertytype2string >>> areLastSegmentsOf s)

-- | Look for a Property on a given ADT, using a criterium.
lookForProperty :: (PropertyType -> Boolean) -> ADT EnumeratedRoleType ~~~> PropertyType
lookForProperty criterium = filter' (ArrayT <<< propertiesOfADT) criterium

propertiesOfRole :: String ~~~> PropertyType
propertiesOfRole s = propertiesOfRole_ (EnumeratedRoleType s) <|> propertiesOfRole_ (CalculatedRoleType s)
  where
    propertiesOfRole_ :: forall r i. RoleClass r i => i ~~~> PropertyType
    propertiesOfRole_ = ArrayT <<< ((getPerspectType :: i -> MonadPerspectives r) >=> properties)

viewsOfRole :: String ~~~> ViewType
viewsOfRole s = f (EnumeratedRoleType s) <|> f (CalculatedRoleType s) where
  f :: forall i r. RoleClass r i => i ~~~> ViewType
  f = ArrayT <<< (getPerspectType >=> views)

propertiesOfView :: ViewType ~~~> PropertyType
propertiesOfView = ArrayT <<< (getPerspectType >=> pure <<< propertyReferences)

lookForUnqualifiedViewType_ :: String -> (EnumeratedRoleType ~~~> ViewType)
lookForUnqualifiedViewType_ s i = lookForView (unwrap >>> areLastSegmentsOf s) (ST i)

lookForUnqualifiedViewType :: String -> (ADT EnumeratedRoleType ~~~> ViewType)
lookForUnqualifiedViewType s = lookForView (unwrap >>> areLastSegmentsOf s)

lookForView :: (ViewType -> Boolean) -> ADT EnumeratedRoleType ~~~> ViewType
lookForView criterium = filter' (ArrayT <<< viewsOfADT) criterium

qualifyRoleInDomain :: String -> String ~~~> RoleType
qualifyRoleInDomain localName namespace = ArrayT do
  DomeinFile {calculatedRoles, enumeratedRoles} <- retrieveDomeinFile namespace
  eCandidates <- pure $ map (ENR <<< EnumeratedRoleType) (filter (\_id -> _id `endsWithSegments` localName) (keys enumeratedRoles))
  cCandidates <- pure $ map (CR <<< CalculatedRoleType) (filter (\_id -> _id `endsWithSegments` localName) (keys calculatedRoles))
  pure $ eCandidates <> cCandidates

qualifyEnumeratedRoleInDomain :: String -> String ~~~> EnumeratedRoleType
qualifyEnumeratedRoleInDomain localName namespace = ArrayT do
  DomeinFile {enumeratedRoles} <- retrieveDomeinFile namespace
  pure $ map EnumeratedRoleType (filter (\_id -> _id `endsWithSegments` localName) (keys enumeratedRoles))

qualifyCalculatedRoleInDomain :: String -> String ~~~> CalculatedRoleType
qualifyCalculatedRoleInDomain localName namespace = ArrayT do
  DomeinFile {calculatedRoles} <- retrieveDomeinFile namespace
  pure $ map CalculatedRoleType (filter (\_id -> _id `endsWithSegments` localName) (keys calculatedRoles))

qualifyContextInDomain :: String -> String ~~~> ContextType
qualifyContextInDomain localName namespace = ArrayT do
  DomeinFile {contexts} <- retrieveDomeinFile namespace
  pure $ map ContextType (filter (\_id -> _id `endsWithSegments` localName) (keys contexts))
