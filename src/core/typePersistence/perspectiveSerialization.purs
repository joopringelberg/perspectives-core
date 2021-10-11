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
import Data.Array (catMaybes, concat, cons, filter, findIndex, foldl, modifyAt, uncons, union)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (Object, fromFoldable)
import Foreign.Object (keys) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, type (~~>), (##=))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Instances.ObjectGetters (getActiveRoleStates_, getActiveStates_)
import Perspectives.Query.QueryTypes (QueryFunctionDescription, domain2roleType, functional, mandatory, range)
import Perspectives.Query.UnsafeCompiler (context2role, getPropertyFunction)
import Perspectives.Representation.Class.Identifiable (displayName, identifier)
import Perspectives.Representation.Class.Property (class PropertyClass)
import Perspectives.Representation.Class.Property (getProperty, isCalculated, functional, mandatory, range, Property(..)) as PROP
import Perspectives.Representation.Class.Role (allProperties, perspectivesOfRoleType)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..), StateSpec(..), PerspectiveId)
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType)
import Perspectives.Representation.Verbs (PropertyVerb, allPropertyVerbs, roleVerbList2Verbs)
import Prelude (bind, eq, flip, map, not, pure, show, ($), (<$>), (<<<), (>=>), (<>))
import Simple.JSON (writeJSON)

type SerialisedPerspective' =
  { id :: String
  , displayName :: String
  , isFunctional :: Boolean
  , isMandatory :: Boolean
  , isCalculated :: Boolean
  , verbs :: Array String
  , properties :: Array SerialisedProperty
  , actions :: Array String
  , roleInstances :: Array RoleInstanceWithProperties
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
  }

newtype SerialisedPerspective = SerialisedPerspective String
derive instance newtypeSerialisedPerspective :: Newtype SerialisedPerspective _

-- | Get the serialisation of the Role instance with
perspectiveForContextAndUser ::
  RoleInstance ->           -- The user role instance
  RoleType ->               -- The user role type
  PerspectiveId ->          -- The String identifying the perspective for the user role type.
  (ContextInstance ~~> SerialisedPerspective)
perspectiveForContextAndUser subject userRoleType perspectiveId cid = ArrayT $ lift do
  contextStates <- map ContextState <$> getActiveStates_ cid
  subjectStates <- map SubjectState <$> getActiveRoleStates_ subject
  allPerspectives <- perspectivesOfRoleType userRoleType
  traverse ((serialisePerspective contextStates subjectStates cid) >=> pure <<< SerialisedPerspective <<< writeJSON) (filter (eq perspectiveId <<< _.id <<< unwrap) allPerspectives)

perspectivesForContextAndUser :: RoleInstance -> RoleType -> (ContextInstance ~~> SerialisedPerspective)
perspectivesForContextAndUser subject userRoleType cid = ArrayT $ lift do
  contextStates <- map ContextState <$> getActiveStates_ cid
  subjectStates <- map SubjectState <$> getActiveRoleStates_ subject
  perspectives <- perspectivesOfRoleType userRoleType
  traverse ((serialisePerspective contextStates subjectStates cid) >=> pure <<< SerialisedPerspective <<< writeJSON) perspectives

serialisePerspective ::
  Array StateSpec ->
  Array StateSpec ->
  ContextInstance ->
  Perspective ->
  MonadPerspectives SerialisedPerspective'
serialisePerspective contextStates subjectStates cid p@(Perspective {id, object, isEnumerated, displayName, roleVerbs, propertyVerbs, actions}) = do
  properties <- serialiseProperties object (concat (catMaybes $ (flip lookup (unwrap propertyVerbs)) <$> (contextStates <> subjectStates)))
  roleInstances <- roleInstancesWithProperties properties cid p
  pure { id
    , displayName
    , isFunctional: pessimistic $ functional object
    , isMandatory: pessimistic $ mandatory object
    , isCalculated: not isEnumerated
    , verbs: show <$> concat (roleVerbList2Verbs <$> (catMaybes $ (flip lookup (unwrap roleVerbs)) <$> (contextStates <> subjectStates)))
    , properties
    , actions: concat (OBJ.keys <$> (catMaybes $ (flip lookup (unwrap actions)) <$> contextStates))
    , roleInstances
    }

serialiseProperties :: QueryFunctionDescription -> Array PropertyVerbs -> MonadPerspectives (Array SerialisedProperty)
serialiseProperties object pverbs = do
  allProps <- allProperties (unsafePartial domain2roleType $ range object)
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
          }

    expandPropertyVerbs :: Array PropertyType -> PropertyVerbs -> Array (Tuple PropertyType (Array PropertyVerb))
    expandPropertyVerbs allProps (PropertyVerbs props verbs) = let
      verbs' = expandVerbs verbs
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
  , objectStateBasedSerialisedProperties :: Array SerialisedProperty
  , propertyValues :: Object ValuesWithVerbs
  }

-- | The verbs in this type contain both those based on context- and subject state,
-- | and those based on object state.
type ValuesWithVerbs =
  { values :: Array String
  , propertyVerbs :: Array String
  }

newtype SerialisedRoleInstanceWithProperties = SerialisedRoleInstanceWithProperties String
derive instance newtypeSerialisedRoleInstanceWithProperties :: Newtype SerialisedRoleInstanceWithProperties _

type Intermediate =
  { id :: String              -- property id
  , getter :: (RoleInstance ~~> Value)
  , verbs :: Array String}

roleInstancesWithProperties ::
  (Array SerialisedProperty) ->
  ContextInstance ->
  Perspective ->
  MonadPerspectives (Array RoleInstanceWithProperties)
roleInstancesWithProperties sps cid (Perspective{object, roleVerbs, propertyVerbs}) = do
  (roleGetter :: ContextInstance ~~> RoleInstance) <- context2role object
  (roleInstances :: Array RoleInstance) <- cid ##= roleGetter
  -- propertyGetters <- pure []
  (propertyGetters :: Array Intermediate) <- for sps
    (\{id, verbs} -> do
      getter <- getPropertyFunction id
      pure $ {id, getter, verbs})
  for roleInstances (roleInstanceWithProperties propertyGetters)
  where
    roleInstanceWithProperties ::
      Array Intermediate ->
      RoleInstance ->
      MonadPerspectives RoleInstanceWithProperties
    roleInstanceWithProperties intermediates roleId = do
      roleStates <- map ObjectState <$> getActiveRoleStates_ roleId
      -- Compute additional RoleVerbs based on the state of the RoleInstance.
      objectStateBasedRoleVerbs <- pure $ show <$> concat (roleVerbList2Verbs <$> (catMaybes $ (flip lookup (unwrap roleVerbs)) <$> roleStates))
      -- Compute additional PropertyVerb-Property combinations based on the state of the RoleInstance.
      (objectStateBasedSerialisedProperties :: (Array SerialisedProperty)) <- serialiseProperties object (concat (catMaybes $ (flip lookup (unwrap propertyVerbs)) <$> roleStates))
      extraIntermediates <- for
        objectStateBasedSerialisedProperties
        (\{id, verbs} -> do
          getter <- getPropertyFunction id
          pure $ {id, getter, verbs})
      -- Add the extra SerialisedProperties to the Array we already had
      (allIntermediates :: Array Intermediate) <- pure $ foldl add intermediates extraIntermediates

      -- Apply each propertygetter to the role instance.
      (valuesAndVerbs :: Array (Tuple String ValuesWithVerbs)) <- for allIntermediates
        \({id, getter, verbs}) -> do
          vals <- (roleId ##= getter)
          pure $ Tuple id
            { values: unwrap <$> vals
            , propertyVerbs: verbs}
      pure
        { roleId: (unwrap roleId)
        , objectStateBasedRoleVerbs
        , objectStateBasedSerialisedProperties
        , propertyValues: fromFoldable valuesAndVerbs
      }
    add :: Array Intermediate ->
      Intermediate ->
      Array Intermediate
    add cumulator n@({id, getter, verbs}) = case findIndex (eq id <<< _.id) cumulator of
      Nothing -> cons n cumulator
      Just i -> unsafePartial fromJust $ modifyAt i (\({verbs: vs}) -> {id, getter, verbs: union vs verbs}) cumulator
