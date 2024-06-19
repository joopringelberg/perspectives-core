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

-- | We store InvertedQueries in four different sets on EnumeratedRoles, and another set in EnumeratedProperty.
-- | Roles can have an __Aspect Context__ that is different from their __Lexical Context__.
-- | We have to take into account this fact when, in runtime, we need to apply an InvertedQuery to a given Delta.
-- | Simply put, a Role in one context is a waystation in different paths than that same role in another Context.
-- |
-- | In this module we analyse the situation for a given pair of role types, and a given set of InvertedQueries.
-- | The result of this analysis is an index that we use to select the right InvertedQueries from a set.

module Perspective.InvertedQuery.Indices where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Data.Array (cons, elemIndex, singleton)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Foreign.Object (lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.ArrayUnions (ArrayUnions(..))
import Perspectives.CoreTypes (MonadPerspectives, MP, (###=))
import Perspectives.Identifiers (startsWithSegments, typeUri2typeNameSpace_)
import Perspectives.Instances.ObjectGetters (context', contextType_)
import Perspectives.InvertedQueryKey (RunTimeInvertedQueryKey(..))
import Perspectives.Query.QueryTypes (QueryFunctionDescription, RoleInContext(..), domain, domain2roleInContext, domain2roleType, queryFunction, range, roleDomain, roleInContext2Role, roleRange)
import Perspectives.Representation.ADT (ADT(..), allLeavesInExpandedADT, computeCollection)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole, getPerspectType)
import Perspectives.Representation.Class.Role (allLocallyRepresentedProperties, bindingOfADT, completeExpandedFillerRestriction, contextOfRepresentation)
import Perspectives.Representation.EnumeratedRole (EnumeratedRole(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.QueryFunction (FunctionName(..), QueryFunction(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedPropertyType(..), EnumeratedRoleType(..), PropertyType(..))
import Perspectives.Types.ObjectGetters (roleAspectsClosure)
import Perspectives.TypesForDeltas (RoleBindingDelta(..))
import Prelude (class Eq, class Ord, Unit, bind, compare, discard, eq, identity, join, map, not, pure, unit, ($), (&&), (/=), (<#>), (<$>), (<<<), (==), (>=>), (>>=))

-- | An InvertedQueryKey is a triplet of types. From a RoleBindingDelta we take the role instances and their
-- | context instances.
-- | But an instance can have many types: its base type and all its Aspects. So from a RoleBindingDelta we can
-- | derive three sets of types.
-- | A InvertedQueryKeyCollection represents all seperate triplets that can be formed from these three type sets.
-- | It is, in fact, a triplet of PRODUCTs of types.
-- | When we look for InvertedQueries on an InvertedQueryMap, we have to traverse (in principle) all its keys
-- | and check whether it is a member of the InvertedQueryKeyCollection, collecting InvertedQueries from matched
-- | keys as we go.
-- | Or, equivalently, we generate all keys in the collection and index the map with each of them, joining the results.

-- | A mutation is performed on instances. However, each instance can have several types (obviously its base type, but also all its Aspect types).
-- | Aspects have not been included in Type Level keys for InvertedQueries. Consequently, we must compute them runtime.
-- | Doing so, we must make sure that we only construct keys that describe existing paths through type space.

-----------------------------------------------------------
-- COMPUTING KEYS IN RUN TIME
-----------------------------------------------------------
-- | Keys for inverted queries from filled to filler.
-- | Query the InvertedQueryDatase with these keys computed from a RoleBindingDelta,
-- | where type SetFirstBinding or ReplaceBinding.
-- | This function starts from a RoleBindingDelta; use `runtimeIndexForFilledQueries'` instead if you have the four types.
-- | Conceptually, the filler fills not only the filled role but also all roles it fills - recursively.
-- | However, we deal with that outside this function. 
runtimeIndexForFilledQueries :: Partial => RoleBindingDelta -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
runtimeIndexForFilledQueries (RoleBindingDelta{filled, filledType:filledRole_destination}) = do 
  filledContext <- context' filled
  filledContext_destination <- contextType_ filledContext
  -- all RoleInContext items in the complete expansion of the declared filler restriction of the filled type, including aspects.
  fillerRoleInContexts <- getEnumeratedRole filledRole_destination >>= completeExpandedFillerRestriction >>= pure <<< maybe [] allLeavesInExpandedADT
  pure $ ArrayUnions $ fillerRoleInContexts <#> \(RoleInContext{role:fillerRole_origin, context: fillerContext_origin}) -> 
    RTFilledKey {fillerRole_origin, fillerContext_origin, filledRole_destination, filledContext_destination}

-- | Keys for queries from filler to filled, exactly like runtimeIndexForFilledQueries.
runtimeIndexForFilledQueries' :: EnumeratedRoleType -> ContextType -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
runtimeIndexForFilledQueries' filledRole_destination filledContext_destination = do
  -- all RoleInContext items in the complete expansion of the declared filler restriction of the filled type, including aspects.
  fillerRoleInContexts <- getEnumeratedRole filledRole_destination >>= completeExpandedFillerRestriction >>= pure <<< maybe [] allLeavesInExpandedADT
  -- Conceptually, the filler fills not only the filled role but also all roles it fills - recursively.
  -- However, we deal with that outside this function. 
  pure $ ArrayUnions $ fillerRoleInContexts <#> \(RoleInContext{role:fillerRole_origin, context: fillerContext_origin}) -> 
    RTFilledKey {fillerRole_origin, fillerContext_origin, filledRole_destination, filledContext_destination}

-- | Keys for inverted queries from filler to filled.
-- | Query the InvertedQueryDatase with these keys computed from a RoleBindingDelta,
-- | with type SetFirstBinding or ReplaceBinding.
runtimeIndexForFillerQueries :: Partial => RoleBindingDelta -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
runtimeIndexForFillerQueries d = do
  keys <- runtimeIndexForFilledQueries d
  pure $ keys <#> \(RTFilledKey{fillerRole_origin, fillerContext_origin, filledRole_destination, filledContext_destination}) -> 
    RTFillerKey { filledRole_origin: filledRole_destination
                , filledContext_origin: filledContext_destination
                , fillerRole_destination: fillerRole_origin
                , fillerContext_destination: fillerContext_origin}

-- | Keys for queries from filler to filled.
runtimeIndexForFillerQueries' :: EnumeratedRoleType -> ContextType -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
runtimeIndexForFillerQueries' filledRole_origin filledContext_origin = do
  -- all RoleInContext items in the complete expansion of the declared filler restriction of the filled type, including aspects.
  fillerRoleInContexts <- getEnumeratedRole filledRole_origin >>= completeExpandedFillerRestriction >>= pure <<< maybe [] allLeavesInExpandedADT
  pure $ ArrayUnions $ fillerRoleInContexts <#> \(RoleInContext{role:fillerRole_destination, context: fillerContext_destination}) -> 
    RTFillerKey {filledRole_origin, filledContext_origin, fillerRole_destination, fillerContext_destination}

-- | Construct the combination of the role type and its aspects 
-- with their lexical contexts, and add to that the combination of the role type and the instantiation context type.
roleContextCombinations :: EnumeratedRoleType -> ContextType -> MonadPerspectives (ArrayUnions (Tuple EnumeratedRoleType ContextType))
roleContextCombinations roleType instantiationContext = do 
  roleTypes <- roleType ###= roleAspectsClosure  
  combinations <- execWriterT $ for roleTypes \role_origin -> do
    lexicalContext <- lift (getPerspectType role_origin >>= pure <<< contextOfRepresentation)
    if lexicalContext == instantiationContext
      then pure unit
      -- Only push this key if it is not a duplicate of the key with the instantiation context!
      else tell [(Tuple role_origin lexicalContext)]
  pure $ ArrayUnions $ cons (Tuple roleType instantiationContext) combinations

-- | Keys for queries from role to context.
-- | Query the InvertedQueryDatase with these keys.
runtimeIndexForContextQueries :: EnumeratedRoleType -> ContextInstance -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
runtimeIndexForContextQueries r c = contextType_ c >>= roleContextCombinations r >>= 
  pure <<< (map \(Tuple role_origin context_destination) -> RTContextKey {role_origin, context_destination})

-- | Keys for queries from context to role.
-- | Query the InvertedQueryDatase with these keys.
runTimeIndexForRoleQueries :: EnumeratedRoleType -> ContextInstance -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
runTimeIndexForRoleQueries r c = contextType_ c >>= roleContextCombinations r >>= pure <<< (map \(Tuple role_destination context_origin) -> RTRoleKey {context_origin, role_destination})

-- | Keys for queries from property to role.
-- | Query the InvertedQueryDatase with these keys.
runtimeIndexForPropertyQueries :: EnumeratedRoleType -> EnumeratedRoleType -> EnumeratedPropertyType -> EnumeratedPropertyType -> MonadPerspectives (Array RunTimeInvertedQueryKey)
runtimeIndexForPropertyQueries typeOfInstanceOnPath typeOfPropertyBearingInstance property replacementProperty = 
  let
    aspectRoleType = EnumeratedRoleType $ typeUri2typeNameSpace_ (unwrap property)
  in
    -- Assuming that we never fill a role type with itself.
    if typeOfInstanceOnPath == typeOfPropertyBearingInstance
      -- The property value is represented on the role instance on the path (1, 3, 5).
      then if property `isAspectPropertyOf` typeOfPropertyBearingInstance
        -- The property is an Aspect property (3, 5).
        then if property /= replacementProperty
          -- A replacement was used for the aspect property (5).
          then pure 
            [ RTPropertyKey { property: replacementProperty, role: typeOfPropertyBearingInstance }
            , RTPropertyKey { property, role: aspectRoleType} ]
          -- No replacement was used for the aspect property (it was not contextualized to the role bearing the Aspect) (3).
          else pure [ RTPropertyKey { property, role: typeOfPropertyBearingInstance }
                    , RTPropertyKey { property, role: aspectRoleType }]
        -- The property is NOT an Aspect property (1)
        else pure [RTPropertyKey {property, role:typeOfPropertyBearingInstance}]
      -- The property value is represented on a filler of the role instance on the path (2, 4, 6)
      else if property `isAspectPropertyOf` typeOfInstanceOnPath
        -- The property is an Aspect property (4, 6).
        then if property /= replacementProperty
          -- A replacement was used for the Aspect property (6)
          then pure [ RTPropertyKey {property: replacementProperty, role: typeOfPropertyBearingInstance} 
                    , RTPropertyKey {property: property, role: aspectRoleType } ]
          -- No replacement was used for the aspect property (it was not contextualized to the role bearing the Aspect) (4).
          else pure [ RTPropertyKey {property, role: typeOfPropertyBearingInstance}
                    , RTPropertyKey {property, role: aspectRoleType} ]
        -- The property is NOT an aspect property (2)
        else pure [RTPropertyKey {property, role: typeOfPropertyBearingInstance}]
  where
    isAspectPropertyOf :: EnumeratedPropertyType -> EnumeratedRoleType -> Boolean
    isAspectPropertyOf (EnumeratedPropertyType propId) (EnumeratedRoleType rtId) = not (propId `startsWithSegments` rtId)

-----------------------------------------------------------
-- COMPUTING KEYS IN COMPILE TIME
-----------------------------------------------------------
-- | Compute the keys for the filled step.
-- | Is exactly like typeLevelKeyForFillerQueries, but with role and domain reversed.
-- | In compile time, we just describe a single step in type space. However, as these types may be compound (ADTs are composed types),
-- | we compute all individual RoleInContext combinations that with certainty describe the origin and those that describe the destination
-- | of the step and then make all combinations.
typeLevelKeyForFilledQueries :: Partial => QueryFunctionDescription -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
typeLevelKeyForFilledQueries qfd | isFilledF (queryFunction qfd) = 
  do 
    -- domain of the qfd represents the filler roles;
    -- All RoleInContext items in the domain of the query.
    (fillerRoleInContexts :: Array RoleInContext) <- pure $ computeCollection singleton (domain2roleInContext $ domain qfd)
    -- range represents the filled roles.
    -- all RoleInContext items in the range of the query.
    (filledRoleInContexts :: Array RoleInContext) <- pure $ computeCollection singleton (domain2roleInContext $ range qfd)
    pure $ ArrayUnions do
      RoleInContext{role:filledRole_destination, context: filledContext_destination} <- filledRoleInContexts
      RoleInContext{role:fillerRole_origin, context: fillerContext_origin} <- fillerRoleInContexts
      pure $ RTFilledKey {fillerRole_origin, fillerContext_origin, filledRole_destination, filledContext_destination}

isFilledF :: QueryFunction -> Boolean
isFilledF qf = case qf of
  FilledF _ _ -> true
  _ -> false

-- | Compute the keys for the filledBy (Filler) step.
-- | Is exactly like typeLevelKeyForFilledQueries, but with role and domain reversed.
typeLevelKeyForFillerQueries :: Partial => QueryFunctionDescription -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
typeLevelKeyForFillerQueries qfd | isFiller (queryFunction qfd)=
  do 
    -- domain of the qfd represents the filler roles;
    -- All RoleInContext items in the domain of the query.
    (fillerRoleInContexts :: Array RoleInContext) <- pure $ computeCollection singleton (domain2roleInContext $ domain qfd)
    -- range represents the filled roles.
    -- all RoleInContext items in the range of the query.
    (filledRoleInContexts :: Array RoleInContext) <- pure $ computeCollection singleton (domain2roleInContext $ range qfd)
    pure $ ArrayUnions do
      RoleInContext{role:filledRole_origin, context: filledContext_origin} <- filledRoleInContexts
      RoleInContext{role:fillerRole_destination, context: fillerContext_destination} <- fillerRoleInContexts
      pure $ RTFillerKey {fillerRole_destination, fillerContext_destination, filledRole_origin, filledContext_origin}
  
isFiller :: QueryFunction -> Boolean
isFiller (DataTypeGetter FillerF) = true
isFiller (DataTypeGetterWithParameter FillerF _) = true
isFiller _ = false

-- | The EnumeratedPropertyType is defined on the EnumeratedRoleType. It is either defined in its namespace, or 
-- | it is added as an Aspect property. In the latter case, if the (Maybe EnumeratedPropertyType) part is a Just value,
-- | the PropertyIdentifier it contains is a local alias for the property, given in the definition of the EnumeratedRoleType.
data PropertyBearer = 
  PropertyBearer 
    EnumeratedPropertyType          -- The final property; a value is stored under this key
    (Maybe EnumeratedPropertyType)  -- The alias; this identifier may have been used in the query
    EnumeratedRoleType              -- The type of the instance bearing the property (this may be a filler of the instance the query is applied to)
    EnumeratedRoleType              -- The type of the instance the query is applied to 

instance Ord PropertyBearer where
  compare (PropertyBearer finalprop1 malias1 bearer1 origin1) (PropertyBearer finalprop2 malias2 bearer2 origin2) = 
    if finalprop1 `eq` finalprop2
      then if malias1 `eq` malias2
        then if bearer1 `eq` bearer2
          then origin1 `compare` origin2
        else compare bearer1 bearer2
      else compare malias1 malias2
    else compare finalprop1 finalprop2
    

instance Eq PropertyBearer where
  eq (PropertyBearer prop1 mprop1 bearingrole1 querysteprole1) (PropertyBearer prop2 mprop2 bearingrole2 querysteprole2) = 
    prop1 == prop2 && 
    mprop1 == mprop2 && 
    bearingrole1 == bearingrole2 &&
    querysteprole1 == querysteprole2

-- | Find the EnumeratedRoleType whose instances can actually store values for the property type.
-- | If the property type turns out to be an alias, return the original property (the final destination or key under which a value will be stored)
-- | and include the alias in a Maybe value.
getPropertyTypeBearingRoleInstances :: EnumeratedPropertyType -> ADT RoleInContext -> MP (ArrayUnions PropertyBearer)
getPropertyTypeBearingRoleInstances prop adt = ArrayUnions <$> execWriterT (descendInFiller adt)
  where 

    descendInFiller :: ADT RoleInContext -> WriterT (Array PropertyBearer) MP Unit
    descendInFiller adt' = do 
      -- Using the Traversable instance, we replace all terminal RoleInContext values with one or zero PropertyBearers. 
      -- We then collect them using foldMapADT (computeCollection) and tell them in the WriterT monad.
      lift ((for adt' ((getEnumeratedRole <<< roleInContext2Role) >=> getPropertyBearers)) >>= pure <<< computeCollection identity) >>= tell
      mfiller <- lift (bindingOfADT adt')
      case mfiller of 
        Nothing -> pure unit
        Just filler -> descendInFiller filler

    getPropertyBearers :: EnumeratedRole -> MP (Array PropertyBearer)
    getPropertyBearers (EnumeratedRole {propertyAliases, id:eroleType}) = case lookup (unwrap prop) propertyAliases of
      Just destination -> pure $ [PropertyBearer destination (Just prop) eroleType eroleType]
      Nothing -> do
        allProps <- allLocallyRepresentedProperties (ST eroleType)
        if isJust $ elemIndex (ENP prop) allProps
          then pure $ [PropertyBearer prop Nothing eroleType eroleType]
          else pure $ []


typeLevelKeyForPropertyQueries :: EnumeratedPropertyType -> QueryFunctionDescription -> MonadPerspectives (Array RunTimeInvertedQueryKey)
typeLevelKeyForPropertyQueries p qfd = do 
  (ArrayUnions propBearers) <- getPropertyTypeBearingRoleInstances p (unsafePartial domain2roleType $ range qfd)
  join <$> for propBearers 
    \(PropertyBearer property replacementProperty typeOfPropertyBearingInstance typeOfInstanceOnPath) -> 
      runtimeIndexForPropertyQueries 
        typeOfInstanceOnPath 
        typeOfPropertyBearingInstance 
        property 
        -- If no property alias was found, use the original property. runtimeIndexForPropertyQueries handles this.
        (maybe property identity replacementProperty)

-- | Add these keys to an inverted query starting on a Role step.
typeLevelKeyForRoleQueries :: QueryFunctionDescription -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
typeLevelKeyForRoleQueries qfd =
  for 
    (computeCollection singleton (unsafePartial roleRange qfd)) 
    (\(RoleInContext{context, role}) -> roleContextCombinations role context) 
  >>= pure <<< (map \(Tuple role_destination context_origin) -> RTRoleKey {context_origin, role_destination}) <<< join <<< ArrayUnions

-- | Add these keys to an inverted query starting on a Context step.
typeLevelKeyForContextQueries :: QueryFunctionDescription -> MonadPerspectives (ArrayUnions RunTimeInvertedQueryKey)
typeLevelKeyForContextQueries qfd = 
  -- Notice that we first collect all leaves and then traverse them with roleContextCombinations. This is because we do not have a Traversable instance of
  -- ADT and this is semantically equivalent.
  for 
    (computeCollection singleton (unsafePartial roleDomain qfd)) 
    (\(RoleInContext{context, role}) -> roleContextCombinations role context) 
  >>= pure <<< (map \(Tuple role_origin context_destination) -> RTContextKey {role_origin, context_destination}) <<< join <<< ArrayUnions
