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
-- along with this program.  If not, see <https:--www.gnu.org/licenses/>.
--
-- Full text of this license can be found in the LICENSE file in the projects root.
-- END LICENSE

-- | This module contains functions to create a JSON structure from a Perspective,
-- | that will be used by the client to build a screen automatically.

module Perspectives.TypePersistence.PerspectiveSerialisation where

import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, concat, cons, elemIndex, filter, filterA, find, findIndex, foldl, head, intersect, modifyAt, uncons, union)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (class Newtype, unwrap)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object, fromFoldable, isEmpty, values, keys)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), MonadPerspectives, AssumptionTracking)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.Identifiers (isExternalRole)
import Perspectives.Instances.ObjectGetters (getActiveRoleStates, getActiveStates)
import Perspectives.Parsing.Arc.AST (PropertyFacet(..))
import Perspectives.Query.QueryTypes (QueryFunctionDescription, domain2roleType, functional, mandatory, range, roleInContext2Role, roleRange)
import Perspectives.Query.UnsafeCompiler (context2role, getDynamicPropertyGetter)
import Perspectives.Representation.ADT (allLeavesInADT)
import Perspectives.Representation.Class.Identifiable (displayName, identifier)
import Perspectives.Representation.Class.PersistentType (getEnumeratedRole)
import Perspectives.Representation.Class.Property (class PropertyClass)
import Perspectives.Representation.Class.Property (getProperty, isCalculated, functional, mandatory, range, Property(..), constrainingFacets) as PROP
import Perspectives.Representation.Class.Role (allProperties, bindingOfADT, perspectivesOfRoleType, roleKindOfRoleType)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..))
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.Representation.TypeIdentifiers (ContextType, PropertyType, RoleKind, RoleType, roletype2string)
import Perspectives.Representation.Verbs (PropertyVerb, RoleVerb(..), allPropertyVerbs, roleVerbList2Verbs)
import Prelude (bind, eq, flip, map, not, pure, show, ($), (<$>), (<<<), (>=>), (<>), (>>=), (||))
import Simple.JSON (writeJSON)

type SerialisedPerspective' =
  { id :: String
  , displayName :: String
  , isFunctional :: Boolean
  , isMandatory :: Boolean
  , isCalculated :: Boolean
  -- The RoleType having the Perspective.
  , userRoleType :: String
  -- The RoleType of the object of the Perspective.
  , roleType :: Maybe String
  , roleKind :: Maybe RoleKind
  -- The ContextInstance in which the roleInstances are embedded as EnumeratedRole instances.
  , contextInstance :: ContextInstance
  , verbs :: Array String
  -- All properties, including those available to some role instance.
  , properties :: Object SerialisedProperty
  , actions :: Array String
  , roleInstances :: Object RoleInstanceWithProperties
  , contextTypesToCreate :: Array ContextType
  , identifyingProperty :: String
  }

-- | Notice that these SerialisedProperties are just those based on context- and subject
-- | State. The roleInstances may contain, per RoleInstanceWithProperties,
-- | SerialisedProperties based on object state. The Property Values contained in those
-- | RoleInstanceWithProperties contain **all** PropertyVerbs (based on all three kinds
-- | of State).
type SerialisedProperty =
  { id :: String
  , displayName :: String
  , isFunctional :: Boolean
  , isMandatory :: Boolean
  , isCalculated :: Boolean
  , range :: String
  , verbs :: Array String
  , constrainingFacets :: PropertyFacets
  }

type PropertyFacets =
  { minLength :: Maybe Int
  , maxLength :: Maybe Int
  , pattern :: Maybe String
  , whiteSpace :: Maybe String
  , enumeration :: Maybe (Array String)
  , maxInclusive :: Maybe String
  , maxExclusive :: Maybe String
  , minInclusive :: Maybe String
  , minExclusive :: Maybe String
  , totalDigits :: Maybe Int
  , fractionDigits :: Maybe Int
  }

newtype SerialisedPerspective = SerialisedPerspective String
derive instance newtypeSerialisedPerspective :: Newtype SerialisedPerspective _

-- | Get the serialisation of the perspective the user role type has on the object role type,
-- | in a given context instance.
perspectiveForContextAndUser ::
  RoleInstance ->           -- The user role instance
  RoleType ->               -- The user role type
  RoleType ->               -- An object role type, will be matched against the Perspective's roleTypes member.
  (ContextInstance ~~> SerialisedPerspective)
perspectiveForContextAndUser subject userRoleType objectRoleType cid = ArrayT do
  contextStates <- map ContextState <$> (runArrayT $ getActiveStates cid)
  subjectStates <- map SubjectState <$> (runArrayT $ getActiveRoleStates subject)
  allPerspectives <- lift$  perspectivesOfRoleType userRoleType
  traverse ((serialisePerspective contextStates subjectStates cid userRoleType) >=> pure <<< SerialisedPerspective <<< writeJSON) (filter (isJust <<< elemIndex objectRoleType <<< _.roleTypes <<< unwrap) allPerspectives)

perspectivesForContextAndUser :: RoleInstance -> RoleType -> (ContextInstance ~~> SerialisedPerspective)
perspectivesForContextAndUser subject userRoleType cid = ArrayT do
  contextStates <- map ContextState <$> (runArrayT $ getActiveStates cid)
  subjectStates <- map SubjectState <$> (runArrayT $ getActiveRoleStates subject)
  -- NOTE that we ignore perspectives that the user role's aspects may have!
  perspectives <- lift $ perspectivesOfRoleType userRoleType
  (traverse (serialisePerspective contextStates subjectStates cid userRoleType) perspectives) >>=
    (filterA sendToClient) >>=
      pure <<< map (SerialisedPerspective <<< writeJSON)
  where
    sendToClient :: SerialisedPerspective' -> AssumptionTracking Boolean
    sendToClient {verbs, roleInstances, properties} = pure $
      if isEmpty roleInstances
        then (isJust $ elemIndex (show Create) verbs) || (isJust $ elemIndex (show CreateAndFill) verbs)
        else (not $ isEmpty properties) ||
          case head $ values roleInstances of
            Nothing -> false
            Just {objectStateBasedSerialisedProperties} -> not $ isEmpty objectStateBasedSerialisedProperties

serialisePerspective ::
  Array StateSpec ->
  Array StateSpec ->
  ContextInstance ->
  RoleType ->
  Perspective ->
  AssumptionTracking SerialisedPerspective'
serialisePerspective contextStates subjectStates cid userRoleType p@(Perspective {id, object, isEnumerated, displayName, roleTypes, roleVerbs, propertyVerbs, actions}) = do
  properties <- lift $ serialiseProperties object (concat (catMaybes $ (flip lookup (unwrap propertyVerbs)) <$> (contextStates <> subjectStates)))
  roleInstances <- roleInstancesWithProperties properties cid p
  roleKind <- lift $ traverse roleKindOfRoleType (head roleTypes)
  -- If the binding of the ADT that is the range of the object QueryFunctionDescription, is an external role,
  -- its context type may be created.
  contextTypesToCreate <- (lift $ allLeavesInADT <<< map roleInContext2Role <$> bindingOfADT (unsafePartial domain2roleType (range object)))
    >>= pure <<< (filter (isExternalRole <<< unwrap))
    >>= lift <<< traverse getEnumeratedRole
    >>= pure <<< map (_.context <<< unwrap)
  identifyingProperty <- computeIdentifyingProperty properties roleInstances
  pure { id
    , displayName
    , isFunctional: pessimistic $ functional object
    , isMandatory: pessimistic $ mandatory object
    , isCalculated: not isEnumerated
    , userRoleType: (roletype2string userRoleType)
    -- NOTE: there can be more than one roleType.
    , roleType: roletype2string <$> head roleTypes
    , contextInstance: cid
    , roleKind
    , verbs: show <$> concat (roleVerbList2Verbs <$> (catMaybes $ (flip lookup (unwrap roleVerbs)) <$> (contextStates <> subjectStates)))
    , properties: fromFoldable ((\prop@({id:propId}) -> Tuple propId prop) <$> (properties <> unifiedProps roleInstances))
    , actions: concat (keys <$> (catMaybes $ (flip lookup (unwrap actions)) <$> (contextStates <> subjectStates)))
    , roleInstances: fromFoldable ((\r@({roleId}) -> Tuple roleId r) <$> roleInstances)
    , contextTypesToCreate
    , identifyingProperty
    }
  where
    nameRegex :: Regex
    nameRegex = unsafeRegex "Name" noFlags

    computeIdentifyingProperty :: Array SerialisedProperty -> Array RoleInstanceWithProperties -> AssumptionTracking String
    computeIdentifyingProperty serialisedProps roleInstances = case find (\property -> test nameRegex property.id) serialisedProps of
        Just n -> pure n.id
        _ -> do
          -- Otherwise, compute the intersection of the props per role instance
          (commonProps :: Array String) <- case uncons (propNames <$> roleInstances) of
            Nothing -> pure []
            Just {head, tail} -> pure $ foldl intersect head tail
          -- Then find an element that matches "Name"
          case find (test nameRegex) commonProps of
            Just n -> pure n
            -- There may be no property shared by all instances that matches "Name";
            Nothing -> case head serialisedProps of
              -- Then we just return the first property that is available because of context- or subject state;
              Just s -> pure s.id
              -- Lacking that,
              Nothing -> case head commonProps of
                -- we return the first common property;
                Just c -> pure c
                -- lacking that we look for the first roleInstance with properties and return the first of those.
                -- Notice that other instances may not have that property in scope because of their state.
                -- This is no problem; the client will just display an un-editable cell.
                Nothing -> case find (not <<< isEmpty) (_.objectStateBasedSerialisedProperties <$> roleInstances) of
                  Just obj -> pure $ unsafePartial fromJust $ head $ keys obj
                  -- Finally, if no role instance has a property, we return the empty string.
                  -- This perspective will only make it to the client to be shown as a Create button, so we
                  -- will not miss the identifying property.
                  Nothing -> pure ""
    propNames :: RoleInstanceWithProperties -> Array String
    propNames {objectStateBasedSerialisedProperties} = keys objectStateBasedSerialisedProperties

    unifiedProps :: Array RoleInstanceWithProperties -> Array SerialisedProperty
    unifiedProps roleInstances = concat $ values <<< _.objectStateBasedSerialisedProperties <$> roleInstances

serialiseProperties :: QueryFunctionDescription -> Array PropertyVerbs -> MonadPerspectives (Array SerialisedProperty)
serialiseProperties object pverbs = do
  allProps <- allProperties (roleInContext2Role <$> (unsafePartial domain2roleType $ range object))
  (x :: Array (Tuple PropertyType (Array PropertyVerb))) <- pure $ concat (expandPropertyVerbs allProps <$> pverbs)
  (y :: Array (Tuple PropertyType (Array PropertyVerb))) <- pure $ case uncons x of
    Just {head, tail} -> foldl add [head] tail
    Nothing -> []
  traverse makeSerialisedProperty y
  where
    makeSerialisedProperty :: Tuple PropertyType (Array PropertyVerb) -> MonadPerspectives SerialisedProperty
    makeSerialisedProperty (Tuple pt verbs) = do
      propType <- PROP.getProperty pt
      case propType of
        (PROP.E prop) -> makeSerialisedProperty' prop
        (PROP.C prop) -> makeSerialisedProperty' prop
      where
        makeSerialisedProperty' :: forall r i. PropertyClass r i => r -> MonadPerspectives SerialisedProperty
        makeSerialisedProperty' propType = do
          isFunctional <- PROP.functional propType
          isMandatory <- PROP.mandatory propType
          isCalculated <- PROP.isCalculated propType
          range <- PROP.range propType
          pure { id: unwrap $ identifier propType
          , displayName: displayName propType
          , isFunctional
          , isMandatory
          , isCalculated
          , range: show range
          , verbs: show <$> verbs
          , constrainingFacets: serialisePropertyFacets $ PROP.constrainingFacets propType
          }

        serialisePropertyFacets :: Array PropertyFacet -> PropertyFacets
        serialisePropertyFacets facets = foldl (\pr facet -> case facet of
          MinLength x -> pr {minLength = Just x}
          MaxLength x -> pr {maxLength = Just x}
          Pattern s -> pr {pattern = Just $ show s}
          WhiteSpace r -> pr {whiteSpace = Just $ show r}
          Enumeration items -> pr {enumeration = Just items}
          MaxInclusive s -> pr {maxInclusive = Just s}
          MinInclusive s -> pr {minInclusive = Just s}
          MaxExclusive s -> pr {maxExclusive = Just s}
          MinExclusive s -> pr {minExclusive = Just s}
          TotalDigits i -> pr {totalDigits = Just i}
          FractionDigits i -> pr {fractionDigits = Just i})
          { minLength: Nothing
          , maxLength: Nothing
          , pattern: Nothing
          , whiteSpace: Nothing
          , enumeration: Nothing
          , maxInclusive: Nothing
          , maxExclusive: Nothing
          , minInclusive: Nothing
          , minExclusive: Nothing
          , totalDigits: Nothing
          , fractionDigits: Nothing
          }
          facets

    -- Pair each PropertyType will all PropertyVerbs available for it.
    expandPropertyVerbs :: Array PropertyType -> PropertyVerbs -> Array (Tuple PropertyType (Array PropertyVerb))
    expandPropertyVerbs allProps (PropertyVerbs props verbs) = let
      (verbs' :: Array PropertyVerb) = expandVerbs verbs
      in (flip Tuple verbs') <$> expandPropSet props
      where
        expandVerbs :: ExplicitSet PropertyVerb -> Array PropertyVerb
        expandVerbs Universal = allPropertyVerbs
        expandVerbs Empty = []
        expandVerbs (PSet as) = as

        expandPropSet :: ExplicitSet PropertyType -> Array PropertyType
        expandPropSet Universal = allProps
        expandPropSet Empty = []
        expandPropSet (PSet as) = as

    -- Replace two Tuples with the same PropertyType with a single Tuple, with the union of their PropertyVerbs.
    add :: Array (Tuple PropertyType (Array PropertyVerb))
      -> (Tuple PropertyType (Array PropertyVerb))
      -> Array (Tuple PropertyType (Array PropertyVerb))
    add cumulator n@(Tuple prop verbs) = case findIndex (eq prop <<< fst) cumulator of
        Nothing -> cons n cumulator
        Just i -> unsafePartial fromJust $ modifyAt i (\(Tuple _ vs) -> (Tuple prop (union vs verbs))) cumulator

-----------------------------------------------------------------------------------------
-- INSTANCES
-----------------------------------------------------------------------------------------
type RoleInstanceWithProperties =
  { roleId :: String
  , objectStateBasedRoleVerbs :: Array String
  , objectStateBasedSerialisedProperties :: Object SerialisedProperty
  , propertyValues :: Object ValuesWithVerbs
  , actions :: Array String
  }

-- | The verbs in this type contain both those based on context- and subject state,
-- | and those based on object state.
type ValuesWithVerbs =
  { values :: Array String
  , propertyVerbs :: Array String
  }

type Intermediate =
  { id :: String              -- property id
  , getter :: (RoleInstance ~~> Value)
  , verbs :: Array String}

roleInstancesWithProperties ::
  (Array SerialisedProperty) ->
  ContextInstance ->
  Perspective ->
  AssumptionTracking (Array RoleInstanceWithProperties)
roleInstancesWithProperties sps cid (Perspective{object, roleVerbs, propertyVerbs, actions}) = do
  (roleGetter :: ContextInstance ~~> RoleInstance) <- lift $ context2role object
  (roleInstances :: Array RoleInstance) <- runArrayT $ roleGetter cid
  -- propertyGetters <- pure []
  (propertyGetters :: Array Intermediate) <- for sps
    (\{id, verbs} -> do
      getter <- lift $ getDynamicPropertyGetter id (roleInContext2Role <$> unsafePartial roleRange object)
      pure $ {id, getter, verbs})
  for roleInstances (roleInstanceWithProperties propertyGetters)
  where
    roleInstanceWithProperties ::
      Array Intermediate ->
      RoleInstance ->
      AssumptionTracking RoleInstanceWithProperties
    roleInstanceWithProperties intermediates roleId = do
      roleStates <- map ObjectState <$> (runArrayT $ getActiveRoleStates roleId)
      -- Compute additional RoleVerbs based on the state of the RoleInstance.
      objectStateBasedRoleVerbs <- pure $ show <$> concat (roleVerbList2Verbs <$> (catMaybes $ (flip lookup (unwrap roleVerbs)) <$> roleStates))
      -- Compute additional PropertyVerb-Property combinations based on the state of the RoleInstance.
      (objectStateBasedSerialisedProperties :: (Array SerialisedProperty)) <- lift $ serialiseProperties object (concat (catMaybes $ (flip lookup (unwrap propertyVerbs)) <$> roleStates))
      extraIntermediates <- for
        objectStateBasedSerialisedProperties
        (\{id, verbs} -> do
          getter <- lift $ getDynamicPropertyGetter id (roleInContext2Role <$> (unsafePartial roleRange object))
          pure $ {id, getter, verbs})
      -- Add the extra SerialisedProperties to the Array we already had
      (allIntermediates :: Array Intermediate) <- pure $ foldl add intermediates extraIntermediates

      -- Apply each propertygetter to the role instance.
      (valuesAndVerbs :: Array (Tuple String ValuesWithVerbs)) <- for allIntermediates
        \({id, getter, verbs}) -> do
          vals <- (runArrayT $ getter roleId)
          pure $ Tuple id
            { values: unwrap <$> vals
            , propertyVerbs: verbs}
      pure
        { roleId: (unwrap roleId)
        , objectStateBasedRoleVerbs
        , objectStateBasedSerialisedProperties: fromFoldable ((\p@{id} -> Tuple id p) <$> objectStateBasedSerialisedProperties)
        , propertyValues: fromFoldable valuesAndVerbs
        , actions: concat (keys <$> (catMaybes $ (flip lookup (unwrap actions)) <$> roleStates))      }
    add :: Array Intermediate ->
      Intermediate ->
      Array Intermediate
    add cumulator n@({id, getter, verbs}) = case findIndex (eq id <<< _.id) cumulator of
      Nothing -> cons n cumulator
      Just i -> unsafePartial fromJust $ modifyAt i (\({verbs: vs}) -> {id, getter, verbs: union vs verbs}) cumulator
