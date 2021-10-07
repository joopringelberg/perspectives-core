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
import Data.Array (catMaybes, concat, cons, findIndex, foldl, modifyAt, uncons, union)
import Data.Map (lookup)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Foreign.Object (keys) as OBJ
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, type (~~>))
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Instances.ObjectGetters (getActiveStates_)
import Perspectives.Query.QueryTypes (QueryFunctionDescription, domain2roleType, functional, mandatory, range)
import Perspectives.Representation.Class.Identifiable (displayName, identifier)
import Perspectives.Representation.Class.Property (class PropertyClass)
import Perspectives.Representation.Class.Property (getProperty, isCalculated, functional, mandatory, range, Property(..)) as PROP
import Perspectives.Representation.Class.Role (allProperties, perspectivesOfRoleType)
import Perspectives.Representation.ExplicitSet (ExplicitSet(..))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance)
import Perspectives.Representation.Perspective (Perspective(..), PropertyVerbs(..))
import Perspectives.Representation.ThreeValuedLogic (pessimistic)
import Perspectives.Representation.TypeIdentifiers (PropertyType, RoleType, StateIdentifier)
import Perspectives.Representation.Verbs (PropertyVerb, allPropertyVerbs, roleVerbList2Verbs)
import Prelude (flip, not, show, ($), (<$>), bind, pure, (<<<), eq, (>=>))
import Simple.JSON (writeJSON)

type SerialisedPerspective' =
  { id :: String
  , displayName :: String
  , isFunctional :: Boolean
  , isMandatory :: Boolean
  , isCalculated :: Boolean
  , verbs :: Array (String)
  , properties :: Array SerialisedProperty
  , actions :: Array String
  }

type SerialisedProperty =
  { id :: String
  , displayName :: String
  , isFunctional :: Boolean
  , isMandatory :: Boolean
  , isCalculated :: Boolean
  , range :: String
  , verbs :: Array (String)
  }

newtype SerialisedPerspective = SerialisedPerspective String
derive instance newtypeSerialisedPerspective :: Newtype SerialisedPerspective _

perspectivesForContextAndUser :: RoleType -> (ContextInstance ~~> SerialisedPerspective)
perspectivesForContextAndUser userRoleType cid = ArrayT $ lift do
  states <- getActiveStates_ cid
  perspectives <- perspectivesOfRoleType userRoleType
  traverse ((serialisePerspective states) >=> pure <<< SerialisedPerspective <<< writeJSON) perspectives

serialisePerspective :: Array StateIdentifier -> Perspective -> MonadPerspectives SerialisedPerspective'
serialisePerspective states p@(Perspective {id, object, isEnumerated, displayName, roleVerbs, propertyVerbs, actions}) = do
  properties <- serialiseProperties object (concat (catMaybes $ (flip lookup (unwrap propertyVerbs)) <$> states))
  pure { id
    , displayName
    , isFunctional: pessimistic $ functional object
    , isMandatory: pessimistic $ mandatory object
    , isCalculated: not isEnumerated
    , verbs: show <$> concat (roleVerbList2Verbs <$> (catMaybes $ (flip lookup (unwrap roleVerbs)) <$> states))
    , properties
    , actions: concat (OBJ.keys <$> (catMaybes $ (flip lookup (unwrap actions)) <$> states))
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
