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

import Control.Monad.Trans.Class (lift)
import Control.Plus (empty, map, (<|>))
import Data.Array (filter, find, findIndex, intersect, null, singleton)
import Data.List (toUnfoldable)
import Data.Map.Internal (keys) as MAP
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Foreign.Object (keys, values)
import Perspectives.CoreTypes (type (~~~>), MonadPerspectives, (###=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (areLastSegmentsOf, endsWithSegments)
import Perspectives.Instances.Combinators (closure_, conjunction)
import Perspectives.Instances.Combinators (filter', filter) as COMB
import Perspectives.Representation.ADT (ADT(..))
import Perspectives.Representation.Class.Action (providesPerspectiveOnProperty, providesPerspectiveOnRole)
import Perspectives.Representation.Class.PersistentType (getAction, getContext, getEnumeratedProperty, getEnumeratedRole, getPerspectType)
import Perspectives.Representation.Class.Role (class RoleClass, actionSet, adtOfRole, allProperties, allRoles, getRole, greaterThanOrEqualTo, propertiesOfADT, roleAspects, roleAspectsBindingADT, typeIncludingAspects, viewsOfADT)
import Perspectives.Representation.Context (Context, roleInContext, contextRole, userRole) as Context
import Perspectives.Representation.Context (contextAspectsADT)
import Perspectives.Representation.EnumeratedProperty (EnumeratedProperty(..))
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.ExplicitSet (hasElementM)
import Perspectives.Representation.InstanceIdentifiers (Value(..))
import Perspectives.Representation.TypeIdentifiers (ActionType, CalculatedRoleType(..), ContextType(..), EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..), RoleType(..), ViewType, propertytype2string, roletype2string)
import Perspectives.Representation.View (propertyReferences)
import Prelude (bind, flip, join, not, pure, show, ($), (<$>), (<<<), (<>), (==), (>=>), (>>=), (>>>))

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND A ROLETYPE WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------
-- | If a role with the given qualified name is available in the Context or its (in)direct aspects,
-- | return it as a RoleType. From the type we can find out its RoleKind, too.
lookForRoleType :: String -> (ContextType ~~~> RoleType)
lookForRoleType s c = (lift $ getContext c) >>= pure <<< contextAspectsADT >>= lookForRoleTypeOfADT s

lookForRoleTypeOfADT :: String -> (ADT ContextType ~~~> RoleType)
lookForRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> ((==) s)) s

-- | As lookForRoleType, but then with a local name that should string-match the end of the qualified name.
lookForUnqualifiedRoleType :: String -> ContextType ~~~> RoleType
lookForUnqualifiedRoleType s c = (lift $ getContext c) >>= pure <<< contextAspectsADT >>= lookForUnqualifiedRoleTypeOfADT s

-- | We simply require the Pattern to match the end of the string.
lookForUnqualifiedRoleTypeOfADT :: String -> ADT ContextType ~~~> RoleType
lookForUnqualifiedRoleTypeOfADT s = lookForRoleOfADT (roletype2string >>> areLastSegmentsOf s) s

-- | Constructs the set of all roles in the ADT (recursing on aspects of Enumerated roles)
-- | and then applies a comparison function to the name passed in and the names of all those roles.
lookForRoleOfADT :: (RoleType -> Boolean) -> String -> ADT ContextType ~~~> RoleType
lookForRoleOfADT criterium rname adt =  ArrayT (allRoles adt >>= pure <<< filter criterium)

----------------------------------------------------------------------------------------
------- FUNCTIONS OPERATING DIRECTLY ON CONTEXT TYPE
----------------------------------------------------------------------------------------
roleInContext :: ContextType ~~~> RoleType
roleInContext = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< Context.roleInContext)

contextRole :: ContextType ~~~> RoleType
contextRole = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< Context.contextRole)

userRole :: ContextType ~~~> RoleType
userRole = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< Context.userRole)

-- | Returns RoleTypes that are guaranteed to be Enumerated.
enumeratedUserRole :: ContextType ~~~> RoleType
enumeratedUserRole =  ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< filter isEnumerated <<< Context.userRole)
  where
    isEnumerated (ENR _) = true
    isEnumerated (CR _) = false

allRoleTypesInContext :: ContextType ~~~> RoleType
allRoleTypesInContext = conjunction roleInContext $ conjunction contextRole userRole'
  where
    userRole' :: ContextType ~~~> RoleType
    userRole' = ArrayT <<< ((getPerspectType :: ContextType -> MonadPerspectives Context.Context) >=> pure <<< Context.userRole)

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND AN ENUMERATEDPROPERTY WORKING FROM STRINGS OR ADT'S
----------------------------------------------------------------------------------------
-- | Look for a Property on a given EnumeratedRoleType (recursing on its
-- | aspects), using a criterium.
lookForPropertyType_ :: String -> (EnumeratedRoleType ~~~> PropertyType)
lookForPropertyType_ s i = (lift $ getRole (ENR i)) >>= lift <<< adtOfRole >>= lookForProperty (propertytype2string >>> ((==) s))

-- | Look for an unqualified Property on a given EnumeratedRoleType
-- | (recursing on its aspects), using a criterium.
lookForUnqualifiedPropertyType_ :: String -> (EnumeratedRoleType ~~~> PropertyType)
lookForUnqualifiedPropertyType_ s i = lookForProperty (propertytype2string >>> areLastSegmentsOf s) (ST i)
-- lookForUnqualifiedPropertyType_ s i = (lift $ getRole (ENR i)) >>= lift <<< adtOfRole >>= lookForProperty (propertytype2string >>> areLastSegmentsOf s)

-- | Look for a Property on a given ADT, using a qualified name, recursing
-- | on aspects of EnumeratedRoleTypes.
-- | Note: use this function to check that the property is actually defined.
lookForPropertyType :: String -> (ADT EnumeratedRoleType ~~~> PropertyType)
lookForPropertyType s = lookForProperty (propertytype2string >>> ((==) s))

-- | Look for a Property on a given ADT, recursing
-- | on aspects of EnumeratedRoleTypes, using the postfix of a name.
lookForUnqualifiedPropertyType :: String -> (ADT EnumeratedRoleType ~~~> PropertyType)
lookForUnqualifiedPropertyType s = lookForProperty (propertytype2string >>> areLastSegmentsOf s)

-- | Look for a Property on a given ADT (recursing on aspects of
-- | Enumerated Roles), using a criterium.
lookForProperty :: (PropertyType -> Boolean) -> ADT EnumeratedRoleType ~~~> PropertyType
lookForProperty criterium adt = ArrayT (allProperties adt >>= pure <<< filter criterium)
-- lookForProperty criterium = COMB.filter' (ArrayT <<< allProperties) criterium

propertiesOfRole :: String ~~~> PropertyType
propertiesOfRole s = propertiesOfRole_ (EnumeratedRoleType s) <|> propertiesOfRole_ (CalculatedRoleType s) <|> empty
  where
    propertiesOfRole_ :: forall r i. RoleClass r i => i ~~~> PropertyType
    propertiesOfRole_ = ArrayT <<< ((getPerspectType :: i -> MonadPerspectives r) >=> roleAspectsBindingADT >=> propertiesOfADT)

----------------------------------------------------------------------------------------
------- FUNCTIONS FOR ASPECTS
----------------------------------------------------------------------------------------
aspectsOfRole :: EnumeratedRoleType ~~~> EnumeratedRoleType
aspectsOfRole = ArrayT <<< (getPerspectType >=> roleAspects)

aspectsClosure :: EnumeratedRoleType ~~~> EnumeratedRoleType
aspectsClosure = closure_ aspectsOfRole

hasAspect :: EnumeratedRoleType -> (EnumeratedRoleType ~~~> Boolean)
hasAspect aspect roleType = ArrayT do
  aspects <- roleType ###= aspectsClosure
  pure [isJust $ findIndex ((==) aspect) aspects]

----------------------------------------------------------------------------------------
------- FUNCTIONS FOR ACTIONS
----------------------------------------------------------------------------------------
actionsOfRole :: EnumeratedRoleType ~~~> ActionType
actionsOfRole rt = ArrayT (getEnumeratedRole rt >>= unwrap >>> _.perspectives >>> values >>> join >>> pure)

actionsClosure :: EnumeratedRoleType ~~~> ActionType
actionsClosure = aspectsClosure >=> actionsOfRole

isAutomatic :: ActionType ~~~> Boolean
isAutomatic at = ArrayT (getAction at >>= unwrap >>> _.executedByBot >>> singleton >>> pure)

----------------------------------------------------------------------------------------
------- FUNCTIONS ON ROLETYPES
----------------------------------------------------------------------------------------
-- | R1 `specialisesRoleType` R2 is true, iff R2 is an (indirect) Aspect of R1 or if both are equal.
-- | We want to use this function as the computation behind the query step `specialisesRoleType`:
-- |  filter <expression that yields role types> with specialisesRoleType SomeRole
-- | The result must be all role types that, indeed, specialise SomeRole.
-- | hence in the QueryCompiler, we flip `specialisesRoleType`, so we can apply it to SomeRole first
-- | and it is still bound to the second parameter.
specialisesRoleType :: RoleType -> (RoleType ~~~> Value)
specialisesRoleType t1 t2 = lift do
  t1' <- typeIncludingAspects t1
  t2' <- typeIncludingAspects t2
  r <- t1' `greaterThanOrEqualTo` t2'
  pure $ Value $ show r

specialisesRoleType_ :: RoleType -> (RoleType -> MonadPerspectives Boolean)
specialisesRoleType_ t1 t2 = do
  t1' <- typeIncludingAspects t1
  t2' <- typeIncludingAspects t2
  t1' `greaterThanOrEqualTo` t2'

----------------------------------------------------------------------------------------
------- FUNCTIONS TO FIND VIEWS
----------------------------------------------------------------------------------------
viewsOfRole :: String ~~~> ViewType
viewsOfRole s = f (EnumeratedRoleType s) <|> f (CalculatedRoleType s) where
  f :: forall i r. RoleClass r i => i ~~~> ViewType
  f = ArrayT <<< (getPerspectType >=> roleAspectsBindingADT >=> viewsOfADT)

propertiesOfView :: ViewType ~~~> PropertyType
propertiesOfView = ArrayT <<< (getPerspectType >=> pure <<< propertyReferences)

lookForUnqualifiedViewType_ :: String -> (EnumeratedRoleType ~~~> ViewType)
lookForUnqualifiedViewType_ s i = (lift $ getRole (ENR i)) >>= lift <<< adtOfRole >>= lookForView (unwrap >>> areLastSegmentsOf s)

lookForUnqualifiedViewType :: String -> (ADT EnumeratedRoleType ~~~> ViewType)
lookForUnqualifiedViewType s = lookForView (unwrap >>> areLastSegmentsOf s)

lookForView :: (ViewType -> Boolean) -> ADT EnumeratedRoleType ~~~> ViewType
lookForView criterium = COMB.filter' (ArrayT <<< viewsOfADT) criterium

----------------------------------------------------------------------------------------
------- FUNCTIONS TO QUALIFY ROLES AND CONTEXTS
----------------------------------------------------------------------------------------
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

----------------------------------------------------------------------------------------
------- USER ROLETYPES WITH A PERSPECTIVE ON A ROLETYPE
----------------------------------------------------------------------------------------
-- TODO: handle Calculated user roles
hasPerspectiveOnRole :: RoleType -> RoleType ~~~> Boolean
hasPerspectiveOnRole (ENR ur) (ENR rt@(EnumeratedRoleType _)) = do
  EnumeratedRole{onContextDelta_context, onContextDelta_role, onRoleDelta_binder, onRoleDelta_binding} <- lift $ getEnumeratedRole rt
  -- The role, or one of its aspects, must be compared to the usertypes in the various
  -- InvertedQueries.
  (userRoleAndAspects :: Array RoleType) <- lift (map ENR <$> (ur ###= aspectsClosure))
  pure $ not $ null $ intersect userRoleAndAspects (join (toUnfoldable <<< MAP.keys <<< _.userTypes <<< unwrap <$> (onContextDelta_context <> onContextDelta_role <> onRoleDelta_binder <> onRoleDelta_binding)))

hasPerspectiveOnRole _ _ = pure false

-- | <RoleType> `roleIsInPerspectiveOf` <userRole>
roleIsInPerspectiveOf :: RoleType -> RoleType ~~~> Boolean
roleIsInPerspectiveOf = flip hasPerspectiveOnRole

-- | In a Context type, find all enumerated local user roles that have a perspective on a given RoleType.
rolesWithPerspectiveOnRole :: RoleType -> ContextType ~~~> RoleType
rolesWithPerspectiveOnRole rt = COMB.filter enumeratedUserRole (roleIsInPerspectiveOfLocalUser rt)
  where
  -- | <RoleType> `roleIsInPerspectiveOfLocalUser` <userRole>
  -- | True iff the userRole or one of its aspects has an Action with RoleType as object.
  roleIsInPerspectiveOfLocalUser :: RoleType -> RoleType ~~~> Boolean
  roleIsInPerspectiveOfLocalUser rt' userRole' = (lift $ typeIncludingAspects userRole') >>= lift <<< actionSet >>= hasElementM g
    where
      g = lift <<< getAction >=> pure <<< providesPerspectiveOnRole rt'

----------------------------------------------------------------------------------------
------- USER ROLETYPES WITH A PERSPECTIVE ON A PROPERTYTYPE
----------------------------------------------------------------------------------------
hasPerspectiveOnProperty :: RoleType -> PropertyType ~~~> Boolean
hasPerspectiveOnProperty (ENR ur) (ENP pt@(EnumeratedPropertyType _)) = do
  EnumeratedProperty{onPropertyDelta} <- lift $ getEnumeratedProperty pt
  (userRoleAndAspects :: Array RoleType) <- lift (map ENR <$> (ur ###= aspectsClosure))
  pure $ not $ null $ intersect userRoleAndAspects (join (toUnfoldable <<< MAP.keys <<< _.userTypes <<< unwrap <$> onPropertyDelta))
hasPerspectiveOnProperty _ _ = pure false

-- | <RoleType> `propertyIsInPerspectiveOf` <userRole>
propertyIsInPerspectiveOf :: PropertyType -> RoleType ~~~> Boolean
propertyIsInPerspectiveOf = flip hasPerspectiveOnProperty

rolesWithPerspectiveOnProperty :: PropertyType -> ContextType ~~~> RoleType
rolesWithPerspectiveOnProperty pt = COMB.filter userRole (propertyIsInPerspectiveOfLocalUser pt)
  where
    propertyIsInPerspectiveOfLocalUser :: PropertyType -> RoleType ~~~> Boolean
    propertyIsInPerspectiveOfLocalUser rt userRole' = (lift $ typeIncludingAspects userRole') >>= lift <<< actionSet >>= hasElementM g
      where
        g = lift <<< getAction >=> lift <<< providesPerspectiveOnProperty rt
