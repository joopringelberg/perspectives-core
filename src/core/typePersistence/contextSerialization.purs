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
import Data.Array (catMaybes, elemIndex, filter, head, length, null, filterA)
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (for, traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), AssumptionTracking, MonadPerspectivesQuery, MPQ, (###=), (##>))
import Perspectives.Data.EncodableMap (lookup)
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (typeUri2ModelUri_)
import Perspectives.Instances.ObjectGetters (getActiveRoleStates, getActiveStates)
import Perspectives.Query.Interpreter (lift2MPQ)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Query.UnsafeCompiler (compileFunction, getRoleInstances)
import Perspectives.Representation.Class.Role (perspectivesOfRoleType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.Perspective (Perspective(..), StateSpec(..))
import Perspectives.Representation.ScreenDefinition (ChatDef(..), ColumnDef(..), FormDef(..), MarkDownDef(..), RowDef(..), ScreenDefinition(..), ScreenElementDef(..), ScreenKey(..), TabDef(..), TableDef(..), WidgetCommonFieldsDef)
import Perspectives.Representation.TypeIdentifiers (ContextType, DomeinFileId(..), RoleType(..))
import Perspectives.TypePersistence.PerspectiveSerialisation (perspectiveForContextAndUserFromId, perspectivesForContextAndUser', serialisePerspective)
import Perspectives.TypePersistence.PerspectiveSerialisation.Data (SerialisedPerspective')
import Perspectives.Types.ObjectGetters (allEnumeratedRoles, aspectsOfRole, contextAspectsClosure)
import Simple.JSON (writeJSON)
import Unsafe.Coerce (unsafeCoerce)

newtype SerialisedScreen = SerialisedScreen String
derive instance newTypeSerialisedScreen :: Newtype SerialisedScreen _

screenForContextAndUser :: RoleInstance -> RoleType -> ContextType -> (ContextInstance ~~> SerialisedScreen)
screenForContextAndUser userRoleInstance userRoleType contextType contextInstance = do
  DomeinFile df <- lift2MPQ $ retrieveDomeinFile (DomeinFileId $ unsafePartial typeUri2ModelUri_ $ unwrap contextType)
  case lookup (ScreenKey contextType userRoleType) df.screens of
    Just s -> populateScreen s
    Nothing -> do 
      -- We should take aspects in consideration!
      -- `userRoleType` may have been added as an aspect user role to `contextType`. In such a case we will never find 
      -- a screen with the key `ScreenKey contextType userRoleType`, but (assuming the aspect added to the contextType is A) we will 
      -- find a screen with `ScreenKey A userRoleType`.
      -- We don't look up screens defined for aspects of a user role type.
      aspects <- lift2MPQ (contextType ###= contextAspectsClosure)
      typesWithScreen <- pure $ filter (\aspect -> isJust $ lookup (ScreenKey aspect userRoleType) df.screens) aspects
      -- TODO. ALS we een aspect scherm overnemen, dan moeten we dat nalopen.
      case head typesWithScreen of 
        Just typeWithScreen -> case lookup (ScreenKey typeWithScreen userRoleType) df.screens of
          Just s -> do 
            -- Dit kan dus niet. Wat wel kan is direct een geserialiseerd perspectief aan de WidgetCommonFieldsDef toevoegen.
            -- dus contextualiseer het perspectief ter plekke en roep dan een variant perspectiveForContextAndUserFromId aan,
            -- die het gecontextualiseerde perspectief inzet.
            -- Het resultaat invoegen in WidgetCommonDieldsDef.
            mscreen <- contextualiseScreen s 
            case mscreen of 
              Nothing -> defaultScreen
              Just contextualisedScreen -> pure $ SerialisedScreen $ writeJSON contextualisedScreen
          Nothing -> defaultScreen
        Nothing -> defaultScreen
  where 
  contextualiseScreen :: ScreenDefinition -> MonadPerspectivesQuery (Maybe ScreenDefinition)
  contextualiseScreen (ScreenDefinition{title, tabs, rows, columns}) = do
    tabs' <- emptyArrayToNothing <<< map catMaybes <$> (for tabs (traverse contextualiseTab))
    rows' <- emptyArrayToNothing <<< map catMaybes <$> (for rows (traverse contextualiseScreenElementDef))
    columns' <- (emptyArrayToNothing <<< map catMaybes) <$> (for columns (traverse contextualiseScreenElementDef))
    pure $ Just $ ScreenDefinition{title, tabs: tabs', rows: rows', columns: columns'}
  
  emptyArrayToNothing :: forall a. Maybe (Array a) -> Maybe (Array a)
  emptyArrayToNothing marr = case marr of 
    Nothing -> Nothing
    Just arr -> if null arr 
      then Nothing
      else Just arr
  
  contextualiseTab :: TabDef -> MPQ (Maybe TabDef)
  contextualiseTab (TabDef {title, isDefault, elements}) = do 
    elements' <- catMaybes <$> (for elements contextualiseScreenElementDef)
    if null elements'
      then pure Nothing 
      else pure $ Just $ TabDef {title, isDefault, elements: elements'}
  
  contextualiseScreenElementDef :: ScreenElementDef -> MPQ (Maybe ScreenElementDef)
  contextualiseScreenElementDef (RowElementD e) = map RowElementD <$> contextualiseRowDef e
  contextualiseScreenElementDef (ColumnElementD e) = map ColumnElementD <$> contextualiseColumnDef e
  contextualiseScreenElementDef (TableElementD e) = map TableElementD <$> contextualiseTableDef e
  contextualiseScreenElementDef (FormElementD e) = map FormElementD <$> contextualiseFormDef e
  contextualiseScreenElementDef (MarkDownElementD e) = map MarkDownElementD <$> contextualiseMarkDownDef e
  contextualiseScreenElementDef (ChatElementD c) = map ChatElementD <$> contextualiseChatDef c 

  contextualiseRowDef :: RowDef -> MPQ (Maybe RowDef)
  contextualiseRowDef (RowDef elements) = do
    elements' <- catMaybes <$> (for elements contextualiseScreenElementDef)
    if null elements'
      then pure Nothing
      else pure $ Just $ RowDef elements'

  contextualiseColumnDef :: ColumnDef -> MPQ (Maybe ColumnDef)
  contextualiseColumnDef (ColumnDef elements) = do
    elements' <- catMaybes <$> (for elements contextualiseScreenElementDef)
    if null elements'
      then pure Nothing 
      else pure $ Just $ ColumnDef elements'

  contextualiseTableDef :: TableDef -> MPQ (Maybe TableDef)
  contextualiseTableDef (TableDef widgetFields) = map TableDef <$> contextualiseWidgetCommonFields widgetFields

  contextualiseFormDef :: FormDef -> MPQ (Maybe FormDef)
  contextualiseFormDef (FormDef widgetFields) = map FormDef <$> contextualiseWidgetCommonFields widgetFields

  contextualiseMarkDownDef :: MarkDownDef -> MPQ (Maybe MarkDownDef)
  contextualiseMarkDownDef md = case md of 
    MarkDownPerspectiveDef {widgetFields, conditionProperty} -> do 
      mwidgetFields <- contextualiseWidgetCommonFields widgetFields
      case mwidgetFields of 
        Just widgetFields' -> pure $ Just $ MarkDownPerspectiveDef {widgetFields: widgetFields', conditionProperty}
        Nothing -> pure Nothing
    _ -> pure $ Just md

  contextualiseChatDef :: ChatDef -> MPQ (Maybe ChatDef)
  contextualiseChatDef (ChatDef r@{chatRole}) = do 
    chatRoleInstance <- (lift $ lift  (contextInstance ##> getRoleInstances chatRole)) 
    pure $ Just $ ChatDef r {chatInstance = chatRoleInstance}
  
  contextualiseWidgetCommonFields :: WidgetCommonFieldsDef -> MPQ (Maybe WidgetCommonFieldsDef)
  contextualiseWidgetCommonFields wc@{perspectiveId, propertyVerbs, roleVerbs, userRole} = do
    contextStates <- lift (map ContextState <$> (runArrayT $ getActiveStates contextInstance))
    subjectStates <- lift (map SubjectState <$> (runArrayT $ getActiveRoleStates userRoleInstance))
    allPerspectives <- lift2MPQ $ perspectivesOfRoleType userRole
    perspective <- pure $ unsafePartial fromJust $ head (filter
      (\(Perspective{id}) -> id == perspectiveId)
      allPerspectives)
    mperspective <- contextualisePerspective perspective
    for mperspective (serialise contextStates subjectStates)
    where 
      serialise :: Array StateSpec -> Array StateSpec -> Perspective -> MPQ WidgetCommonFieldsDef
      serialise contextStates subjectStates perspective = do 
        serialisedPerspective <- lift $ serialisePerspective contextStates subjectStates contextInstance userRole propertyVerbs roleVerbs perspective
        pure $ wc {perspective = Just serialisedPerspective}
  
  contextualisePerspective :: Perspective -> MPQ (Maybe Perspective)
  contextualisePerspective p@(Perspective pr) = if pr.isEnumerated 
    then do 
      -- since the perspective is enumerated, we know there is but a single, EnumeratedRoleType in `roleTypes`.
      roleType <- pure (unsafePartial fromJust $ head pr.roleTypes) >>= unsafePartial case _ of ENR roleType -> pure roleType
      allRoles <- lift2MPQ (contextType ###= allEnumeratedRoles)
      if isJust $ elemIndex roleType allRoles
        -- The context has the roleType, probably as an aspect role.
        then pure $ Just p 
        -- find a role in contextType that has roleType as aspect
        else do 
          rolesWithAspect <- filterA 
            (\erole -> do
              aspects <- lift2MPQ (erole ###= aspectsOfRole)
              pure $ isJust $ elemIndex roleType aspects)
            allRoles
          case head rolesWithAspect of 
            -- No role has the type we're looking for. This perspective should not be used in the screen.
            Nothing -> pure Nothing
            -- This role has roleType as aspect. Contextualise the perspective.
            Just roleWithAspect -> pure $ Just $ Perspective pr { roleTypes = [ENR roleWithAspect], displayName = show roleWithAspect }
    -- A calculated perspective may work. We cannot say.
    else pure $ Just p

  populateScreen :: ScreenDefinition -> MonadPerspectivesQuery SerialisedScreen
  populateScreen s = do 
    -- Now populate the screen definition with instance data.
    (screenInstance :: ScreenDefinition) <- lift $ addPerspectives s userRoleInstance contextInstance
    pure $ SerialisedScreen $ writeJSON screenInstance
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
          , isDefault: false
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
  addPerspectives (ChatElementD re) user ctxt = ChatElementD <$> addPerspectives re user ctxt

instance addPerspectivesTabDef  :: AddPerspectives TabDef where
  addPerspectives (TabDef r) user ctxt = do
    elements <- traverse (\a -> addPerspectives a user ctxt) r.elements
    pure $ TabDef {title: r.title, isDefault: r.isDefault, elements}

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
  
instance AddPerspectives ChatDef where
  addPerspectives (ChatDef r@ {chatRole}) user ctxt = do 
    chatRoleInstance <- (lift (ctxt ##> getRoleInstances chatRole)) 
    pure $ ChatDef r {chatInstance = chatRoleInstance}

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
