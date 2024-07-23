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

module Perspectives.TypePersistence.ContextSerialisation where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, cons, filter, head, length)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), AssumptionTracking, MonadPerspectivesQuery, (###=))
import Perspectives.Data.EncodableMap (lookup)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (typeUri2ModelUri_)
import Perspectives.Query.Interpreter (lift2MPQ)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Query.UnsafeCompiler (compileFunction)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.ScreenDefinition (ColumnDef(..), FormDef(..), MarkDownDef(..), RowDef(..), ScreenDefinition(..), ScreenElementDef(..), ScreenKey(..), TabDef(..), TableDef(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, DomeinFileId(..), RoleType)
import Perspectives.TypePersistence.PerspectiveSerialisation (perspectiveForContextAndUserFromId, perspectivesForContextAndUser')
import Perspectives.TypePersistence.PerspectiveSerialisation.Data (SerialisedPerspective')
import Perspectives.Types.ObjectGetters (contextAspectsClosure)
import Simple.JSON (writeJSON)
import Unsafe.Coerce (unsafeCoerce)

newtype SerialisedScreen = SerialisedScreen String
derive instance newTypeSerialisedScreen :: Newtype SerialisedScreen _

screenForContextAndUser :: RoleInstance -> RoleType -> ContextType -> (ContextInstance ~~> SerialisedScreen)
screenForContextAndUser userRoleInstance userRoleType contextType contextInstance = do
  DomeinFile df <- lift2MPQ $ retrieveDomeinFile (DomeinFileId $ unsafePartial typeUri2ModelUri_ $ unwrap contextType)
  -- We should take aspects in consideration!
  -- `userRoleType` may have been added as an aspect user role to `contextType`. In such a case we will never find 
  -- a screen with the key `ScreenKey contextType userRoleType`, but (assuming the aspect added to the contextType is A) we will 
  -- find a screen with `ScreenKey A userRoleType`.
  -- We don't look up screens defined for aspects of a user role type.
  aspects <- lift2MPQ (contextType ###= contextAspectsClosure)
  typesWithScreen <- pure $ filter (\aspect -> isJust $ lookup (ScreenKey aspect userRoleType) df.screens) (cons contextType aspects)
  case head typesWithScreen of 
    Just typeWithScreen -> case lookup (ScreenKey typeWithScreen userRoleType) df.screens of
      Just s -> do
        -- Now populate the screen definition with instance data.
        (screenInstance :: ScreenDefinition) <- lift $ addPerspectives s userRoleInstance contextInstance
        pure $ SerialisedScreen $ writeJSON screenInstance
      Nothing -> defaultScreen
    Nothing -> defaultScreen
  where 
  defaultScreen :: MonadPerspectivesQuery SerialisedScreen
  defaultScreen = do
    screenInstance <- lift $ constructDefaultScreen userRoleInstance userRoleType contextInstance
    pure $ SerialisedScreen $ writeJSON screenInstance

-- | A screen with a tab for each perspective the user has in this context.
constructDefaultScreen :: RoleInstance -> RoleType -> ContextInstance -> AssumptionTracking ScreenDefinition
constructDefaultScreen userRoleInstance userRoleType cid = do
  (perspectives :: Array SerialisedPerspective') <- runArrayT $ perspectivesForContextAndUser' userRoleInstance userRoleType cid
  -- If there is just a single perspective, don't make tabs but make a row with a widget instead.
  if length perspectives == 1
    then do 
      row <- pure $ Just $ makeRow <$> perspectives
      pure $ ScreenDefinition
        { title: Nothing
        , tabs: Nothing
        , rows: row
        , columns: Nothing
        }
    else do
      tabs <- pure $ Just $ makeTab <$> perspectives
      pure $ ScreenDefinition
        { title: Nothing
        , tabs
        , rows: Nothing
        , columns: Nothing
        }
    where
      makeTab :: SerialisedPerspective' -> TabDef
      makeTab p@{displayName, isFunctional} =
        let
          widgetCommonFields =
            { title: Nothing
            , perspective: Just p
            , perspectiveId: ""
            , propertyVerbs: Nothing
            , roleVerbs: Nothing
            , userRole: userRoleType
            }
          element = if isFunctional
            then FormElementD $ FormDef widgetCommonFields
            else TableElementD $ TableDef widgetCommonFields
        in TabDef
          { title: displayName
          , elements: [
            RowElementD (RowDef [ element ])
          ]}
      makeRow :: SerialisedPerspective' -> ScreenElementDef
      makeRow p@{displayName, isFunctional} = let
          widgetCommonFields =
            { title: Nothing
            , perspective: Just p
            , perspectiveId: ""
            , propertyVerbs: Nothing
            , roleVerbs: Nothing
            , userRole: userRoleType
            }
        in 
          if isFunctional
            then FormElementD $ FormDef widgetCommonFields
            else TableElementD $ TableDef widgetCommonFields

-----------------------------------------------------------
-- CLASS ADDPERSPECTIVES
-----------------------------------------------------------
class AddPerspectives a where
  addPerspectives :: a -> RoleInstance -> ContextInstance -> AssumptionTracking a

instance addPerspectivesScreenDefinition :: AddPerspectives ScreenDefinition where
  addPerspectives (ScreenDefinition r) user ctxt = do
    tabs <- case r.tabs of
      Nothing -> pure Nothing
      Just t -> Just <$> traverse (\a -> addPerspectives a user ctxt) t
    rows <- case r.rows of
      Nothing -> pure Nothing
      Just t -> Just <$> traverse (\a -> addPerspectives a user ctxt) t
    columns <- case r.columns of
      Nothing -> pure Nothing
      Just t -> Just <$> traverse (\a -> addPerspectives a user ctxt) t
    pure $ ScreenDefinition {title: r.title, tabs, rows, columns}

instance addPerspectivesScreenElementDef  :: AddPerspectives ScreenElementDef where
  addPerspectives (RowElementD re) user ctxt = RowElementD <$> addPerspectives re user ctxt
  addPerspectives (ColumnElementD re) user ctxt = ColumnElementD <$> addPerspectives re user ctxt
  addPerspectives (TableElementD re) user ctxt = TableElementD <$> addPerspectives re user ctxt
  addPerspectives (FormElementD re) user ctxt = FormElementD <$> addPerspectives re user ctxt
  addPerspectives (MarkDownElementD re) user ctxt = MarkDownElementD <$> addPerspectives re user ctxt

instance addPerspectivesTabDef  :: AddPerspectives TabDef where
  addPerspectives (TabDef r) user ctxt = do
    elements <- traverse (\a -> addPerspectives a user ctxt) r.elements
    pure $ TabDef {title: r.title, elements}

instance addPerspectivesColumnDef  :: AddPerspectives ColumnDef where
  addPerspectives (ColumnDef cols) user ctxt = do
    cols' <- traverse (traverseScreenElement user ctxt) cols
    pure $ ColumnDef (catMaybes cols')

instance addPerspectivesRowDef  :: AddPerspectives RowDef where
  addPerspectives (RowDef cols) user ctxt = do
    cols' <- traverse (traverseScreenElement user ctxt) cols
    -- A MarkDownDef with a condition that fails should not be in the end result.
    pure $ RowDef (catMaybes cols')

instance addPerspectivesTableDef  :: AddPerspectives TableDef where
  addPerspectives (TableDef widgetCommonFields) user ctxt = do
    perspective <- perspectiveForContextAndUserFromId
      user
      widgetCommonFields
      ctxt
    pure $ TableDef widgetCommonFields {perspective = Just perspective}

instance addPerspectivesFormDef  :: AddPerspectives FormDef where
  addPerspectives (FormDef widgetCommonFields) user ctxt = do
    perspective <- perspectiveForContextAndUserFromId
      user
      widgetCommonFields
      ctxt
    pure $ FormDef widgetCommonFields {perspective = Just perspective}

instance AddPerspectives MarkDownDef where
  addPerspectives md@(MarkDownConstantDef _) user ctxt = pure md
  addPerspectives md@(MarkDownExpressionDef _) user ctxt = pure md
  addPerspectives (MarkDownPerspectiveDef {widgetFields, conditionProperty}) user ctxt = do
    perspective <- perspectiveForContextAndUserFromId
      user
      widgetFields
      ctxt
    pure $ MarkDownPerspectiveDef {widgetFields: widgetFields {perspective = Just perspective}, conditionProperty}

traverseScreenElement :: RoleInstance -> ContextInstance -> ScreenElementDef -> AssumptionTracking (Maybe ScreenElementDef)
traverseScreenElement user ctxt a = case a of 
      MarkDownElementD mddef -> map MarkDownElementD <$> unsafePartial case mddef of 
        -- We transform the markdown string to html client side.
        sed@(MarkDownConstantDef {condition}) -> conditionally condition (pure $ Just sed)
        MarkDownExpressionDef {textQuery, condition} -> conditionally condition 
          do 
            (textGetter :: ContextInstance ~~> Value) <- lift $ unsafeCoerce compileFunction textQuery
            (textA :: Array Value) <- runArrayT $ textGetter ctxt
            pure $ Just $ MarkDownExpressionDef {textQuery, condition, text: maybe Nothing (Just <<< unwrap) (head textA)}
        MarkDownPerspectiveDef {widgetFields, conditionProperty} -> 
          do 
            perspective <- perspectiveForContextAndUserFromId
              user
              widgetFields
              ctxt
            pure $ Just $ MarkDownPerspectiveDef { widgetFields: widgetFields {perspective = Just perspective}, conditionProperty}
      (other :: ScreenElementDef) -> Just <$> addPerspectives other user ctxt

    where
      conditionally :: Maybe QueryFunctionDescription -> AssumptionTracking (Maybe MarkDownDef) -> AssumptionTracking (Maybe MarkDownDef)
      conditionally condition f = case condition of 
        Nothing -> f
        Just c -> do
          (criterium :: ContextInstance ~~> Value) <- lift $ unsafeCoerce compileFunction c
          -- evaluate the condition in the current context
          shouldBeShown <- runArrayT $ criterium ctxt
          case head shouldBeShown of 
            Just (Value "true") -> f
            -- The condition should be strictly true.
            _ -> pure $ Nothing
