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

import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, concat, filter, head, length)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (type (~~>), AssumptionTracking, MonadPerspectivesQuery, (###=), (##=))
import Perspectives.Data.EncodableMap (lookup)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..), runArrayT)
import Perspectives.DomeinCache (retrieveDomeinFile)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Identifiers (typeUri2ModelUri_)
import Perspectives.Instances.ObjectGetters (contextType_)
import Perspectives.ModelTranslation (translationOf)
import Perspectives.Query.Interpreter (lift2MPQ)
import Perspectives.Query.QueryTypes (QueryFunctionDescription)
import Perspectives.Query.UnsafeCompiler (compileFunction, getRoleInstances)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value(..))
import Perspectives.Representation.ScreenDefinition (ChatDef(..), ColumnDef(..), FormDef(..), MarkDownDef(..), RowDef(..), ScreenDefinition(..), ScreenElementDef(..), ScreenKey(..), TabDef(..), TableDef(..), TableFormDef(..), What(..), WhoWhatWhereScreenDef(..))
import Perspectives.Representation.TypeIdentifiers (ContextType, DomeinFileId(..), RoleKind(..), RoleType, roletype2string)
import Perspectives.TypePersistence.PerspectiveSerialisation (perspectiveForContextAndUser', perspectiveForContextAndUserFromId, perspectivesForContextAndUser')
import Perspectives.TypePersistence.PerspectiveSerialisation.Data (SerialisedPerspective')
import Perspectives.TypePersistence.ScreenContextualisation (contextualiseScreen, contextualiseTableFormDef)
import Perspectives.Types.ObjectGetters (contextAspectsClosure)
import Simple.JSON (writeJSON)
import Unsafe.Coerce (unsafeCoerce)

newtype SerialisedScreen = SerialisedScreen String
derive instance newTypeSerialisedScreen :: Newtype SerialisedScreen _

-- | Get the screen for a context and a user. By construction, the whoWhatWhereScreen is Nothing in the result.
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
            mscreen <- runReaderT (contextualiseScreen s) {userRoleInstance, contextType, contextInstance} 
            case mscreen of  
              Nothing -> defaultScreen
              Just contextualisedScreen -> pure $ SerialisedScreen $ writeJSON contextualisedScreen
          Nothing -> defaultScreen
        Nothing -> defaultScreen
  where 

  populateScreen :: ScreenDefinition -> MonadPerspectivesQuery SerialisedScreen
  populateScreen s@(ScreenDefinition {title, tabs, rows, columns, whoWhatWhereScreen}) = do 
    if isJust whoWhatWhereScreen
      then do
        -- Now populate the screen definition with instance data.
        (screenInstance :: ScreenDefinition) <- lift $ addPerspectives s userRoleInstance contextInstance
        pure $ SerialisedScreen $ writeJSON screenInstance
      else do
        (perspectives :: Array SerialisedPerspective') <- lift $ lift (contextInstance ##= perspectivesForContextAndUser' userRoleInstance userRoleType)
        who <- pure $ makeTableFormDef userRoleType <$> filter isOnUserRole perspectives
        whereto <- pure $ makeTableFormDef userRoleType <$> filter isOnContextRole perspectives
        constructedScreen <- lift $ addPerspectives (ScreenDefinition
          { title
          , tabs: Nothing
          , rows: Nothing
          , columns: Nothing
          , whoWhatWhereScreen: Just $ WhoWhatWhereScreenDef $ 
            { who
            , what: FreeFormScreen {tabs, rows, columns}
            , whereto
            }
          })
          userRoleInstance
          contextInstance
        pure $ SerialisedScreen $ writeJSON constructedScreen
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
      who <- pure $ makeTableFormDef userRoleType <$> filter isOnUserRole perspectives
      whereto <- pure $ makeTableFormDef userRoleType <$> filter isOnContextRole perspectives
      pure $ ScreenDefinition
        { title: Nothing
        , tabs: Nothing
        , rows: Nothing
        , columns: Nothing
        , whoWhatWhereScreen: Just $ WhoWhatWhereScreenDef $ 
          { who
          , what: FreeFormScreen {tabs: Nothing, rows: row, columns: Nothing}
          , whereto
          }
        }
    else do
      -- Make tabs just for the thing roles.
      tabs <- pure $ Just $ makeTab <$> filter isOnThingRole perspectives
      who <- pure $ makeTableFormDef userRoleType <$> filter isOnUserRole perspectives
      whereto <- pure $ makeTableFormDef userRoleType <$> filter isOnContextRole perspectives
      pure $ ScreenDefinition
        { title: Nothing
        , tabs: Nothing
        , rows: Nothing
        , columns: Nothing
        , whoWhatWhereScreen: Just $ WhoWhatWhereScreenDef $ 
          { who
          , what: FreeFormScreen {tabs, rows: Nothing, columns: Nothing}
          , whereto
          }
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

isOnThingRole :: SerialisedPerspective' -> Boolean
isOnThingRole {roleKind} = case roleKind of
  Just RoleInContext -> true
  Just ExternalRole -> true
  Just Public -> true
  _ -> false
isOnUserRole :: SerialisedPerspective' -> Boolean
isOnUserRole {roleKind} = case roleKind of
  Just UserRole -> true
  _ -> false
isOnContextRole :: SerialisedPerspective' -> Boolean
isOnContextRole {roleKind} = case roleKind of
  Just ContextRole -> true
  _ -> false
makeTableFormDef :: RoleType -> SerialisedPerspective' -> TableFormDef
makeTableFormDef userRoleType p@{id, displayName} = let
    widgetCommonFields =
      { title: Just displayName
      , perspective: Just p
      , perspectiveId: id
      , propertyVerbs: Nothing
      , roleVerbs: Nothing
      , userRole: userRoleType
      }
  in TableFormDef
    { table: TableDef widgetCommonFields
    , form: FormDef widgetCommonFields
    }

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
    whoWhatWhereScreen <- case r.whoWhatWhereScreen of 
      Nothing -> pure Nothing
      Just t -> Just <$> addPerspectives t user ctxt
    pure $ ScreenDefinition {title: r.title, tabs, rows, columns, whoWhatWhereScreen}

instance AddPerspectives WhoWhatWhereScreenDef where
  addPerspectives (WhoWhatWhereScreenDef r) user ctxt = do
    who <- traverse (\a -> addPerspectives a user ctxt) r.who
    what <- addPerspectives r.what user ctxt
    whereto <- traverse (\a -> addPerspectives a user ctxt) r.whereto
    pure $ WhoWhatWhereScreenDef {who, what, whereto}

instance AddPerspectives What where
  addPerspectives (TableForms tableFormDefs) user ctxt = TableForms <$> traverse (\a -> addPerspectives a user ctxt) tableFormDefs
  addPerspectives (FreeFormScreen {tabs, rows, columns}) user ctxt = do
    tabs' <- case tabs of
      Nothing -> pure Nothing
      Just t -> Just <$> traverse (\a -> addPerspectives a user ctxt) t
    rows' <- case rows of
      Nothing -> pure Nothing
      Just t -> Just <$> traverse (\a -> addPerspectives a user ctxt) t
    columns' <- case columns of
      Nothing -> pure Nothing
      Just t -> Just <$> traverse (\a -> addPerspectives a user ctxt) t
    pure $ FreeFormScreen {tabs: tabs', rows: rows', columns: columns'}

instance AddPerspectives TableFormDef where
  addPerspectives (TableFormDef {table, form}) user ctxt = do
    table' <- addPerspectives table user ctxt
    form' <- addPerspectives form user ctxt
    pure $ TableFormDef {table: table', form: form'}

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
    contextType <- lift $ contextType_ ctxt
    (translatedTitle :: String) <- lift (translationOf (unsafePartial typeUri2ModelUri_ $ unwrap contextType) r.title) 
    pure $ TabDef {title: translatedTitle, isDefault: r.isDefault, elements}

instance addPerspectivesColumnDef  :: AddPerspectives ColumnDef where
  addPerspectives (ColumnDef cols) user ctxt = do
    cols' <- traverse (traverseScreenElement user ctxt) cols
    pure $ ColumnDef (catMaybes cols')

instance addPerspectivesRowDef  :: AddPerspectives RowDef where
  addPerspectives (RowDef rows) user ctxt = do
    rows' <- traverse (traverseScreenElement user ctxt) rows
    -- A MarkDownDef with a condition that fails should not be in the end result.
    pure $ RowDef (catMaybes rows')

instance addPerspectivesTableDef  :: AddPerspectives TableDef where
  addPerspectives (TableDef widgetCommonFields) user ctxt = do
    perspective <- perspectiveForContextAndUserFromId
      user
      widgetCommonFields
      ctxt
    contextType <- lift $ contextType_ ctxt
    (translatedTitle :: Maybe String) <- lift $ traverse (translationOf (unsafePartial typeUri2ModelUri_ $ unwrap contextType)) widgetCommonFields.title
    pure $ TableDef widgetCommonFields {perspective = Just perspective, title = translatedTitle}

instance addPerspectivesFormDef  :: AddPerspectives FormDef where
  addPerspectives (FormDef widgetCommonFields) user ctxt = do
    perspective <- perspectiveForContextAndUserFromId
      user
      widgetCommonFields
      ctxt
    contextType <- lift $ contextType_ ctxt
    (translatedTitle :: Maybe String) <- lift $ traverse (translationOf (unsafePartial typeUri2ModelUri_ $ unwrap contextType)) widgetCommonFields.title
    pure $ FormDef widgetCommonFields {perspective = Just perspective, title = translatedTitle}

instance AddPerspectives MarkDownDef where
  addPerspectives (MarkDownConstantDef r@{text, domain}) user ctxt = do 
    translatedText <- lift $ translationOf domain text
    pure $ MarkDownConstantDef r {text = translatedText}
  addPerspectives md@(MarkDownExpressionDef _) user ctxt = pure md
  addPerspectives (MarkDownPerspectiveDef {widgetFields, conditionProperty}) user ctxt = do
    perspective <- perspectiveForContextAndUserFromId
      user
      widgetFields
      ctxt
    pure $ MarkDownPerspectiveDef {widgetFields: widgetFields {perspective = Just perspective}, conditionProperty}
  
instance AddPerspectives ChatDef where
  addPerspectives (ChatDef r@ {chatRole}) user ctxt = do 
    chatRoleInstance <- runArrayT (getRoleInstances chatRole ctxt)
    pure $ ChatDef r {chatInstance = head chatRoleInstance}

traverseScreenElement :: RoleInstance -> ContextInstance -> ScreenElementDef -> AssumptionTracking (Maybe ScreenElementDef)
traverseScreenElement user ctxt a = case a of 
      MarkDownElementD mddef -> map MarkDownElementD <$> unsafePartial case mddef of 
        -- We transform the markdown string to html client side.
        MarkDownConstantDef r@{text, domain, condition} -> do 
          translatedText <- lift $ translationOf domain text
          conditionally condition (pure $ Just $ MarkDownConstantDef r {text = translatedText})
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

-----------------------------------------------------------
-- GET TABLEFORM
----------------------------------------------------------- 
serialisedTableFormForContextAndUser :: RoleInstance -> RoleType -> ContextType -> RoleType -> (ContextInstance ~~> SerialisedTableForm)
serialisedTableFormForContextAndUser userRoleInstance userRoleType contextType objectRoleType = 
  tableFormForContextAndUser userRoleInstance userRoleType contextType objectRoleType >=> pure <<< SerialisedTableForm <<< writeJSON

tableFormForContextAndUser :: RoleInstance -> RoleType -> ContextType -> RoleType -> (ContextInstance ~~> TableFormDef)
tableFormForContextAndUser userRoleInstance userRoleType contextType objectRoleType contextInstance =  do
  DomeinFile df <- lift2MPQ $ retrieveDomeinFile (DomeinFileId $ unsafePartial typeUri2ModelUri_ $ unwrap contextType)
  case lookup (ScreenKey contextType userRoleType) df.screens of
    Just (ScreenDefinition {whoWhatWhereScreen}) -> case whoWhatWhereScreen of
      -- If there is a whoWhatWhereScreen element, populate it
      Just whowhatwhere -> populateTableForm whowhatwhere
      Nothing -> defaultTableForm
    Nothing -> do 
      -- We should take aspects in consideration!
      -- `userRoleType` may have been added as an aspect user role to `contextType`. In such a case we will never find 
      -- a screen with the key `ScreenKey contextType userRoleType`, but (assuming the aspect added to the contextType is A) we will 
      -- find a screen with `ScreenKey A userRoleType`.
      -- We don't look up screens defined for aspects of a user role type.
      aspects <- lift2MPQ (contextType ###= contextAspectsClosure)
      typesWithScreen <- pure $ filter (\aspect -> isJust $ lookup (ScreenKey aspect userRoleType) df.screens) aspects
      case head typesWithScreen of 
        Just typeWithScreen -> case lookup (ScreenKey typeWithScreen userRoleType) df.screens of
          Just (ScreenDefinition {whoWhatWhereScreen}) -> case whoWhatWhereScreen of
            -- If there is a whoWhatWhereScreen element, contextualise the relevant part of it
            Just whowhatwhere -> contextualiseWhoWhatWhereScreen whowhatwhere
            Nothing -> defaultTableForm
          Nothing -> defaultTableForm
        Nothing -> defaultTableForm
  where 
  defaultTableForm :: MonadPerspectivesQuery TableFormDef
  defaultTableForm = constructDefaultTableForm userRoleInstance userRoleType objectRoleType contextInstance

  populateTableForm :: WhoWhatWhereScreenDef -> MonadPerspectivesQuery TableFormDef
  populateTableForm (WhoWhatWhereScreenDef {what}) = ArrayT case what of 
    TableForms tableFormDefs -> traverse (\a -> addPerspectives a userRoleInstance contextInstance) tableFormDefs
    _ -> pure []

  -- We are looking for a particular object RoleType. It could be in the who, what or whereto elements of the WhoWhatWhereScreenDef.
  -- If the what is a FreeFormScreen, we cannot return anything.
  contextualiseWhoWhatWhereScreen :: WhoWhatWhereScreenDef -> MonadPerspectivesQuery TableFormDef
  contextualiseWhoWhatWhereScreen (WhoWhatWhereScreenDef {who, what, whereto}) = ArrayT do
    (who' :: Array TableFormDef) <- pure $ filter tableFormDefIsForRoleType who
    what' <- case what of 
      FreeFormScreen _ -> pure []
      TableForms tableFormDefs -> pure $ filter tableFormDefIsForRoleType tableFormDefs
    whereto' <- pure $ filter tableFormDefIsForRoleType whereto
    (x :: Array (Array (Maybe TableFormDef))) <- runArrayT $ runReaderT (traverse contextualiseTableFormDef (who' <> what' <> whereto')) {userRoleInstance, contextType, contextInstance}
    pure $ catMaybes $ concat x

    where

    tableFormDefIsForRoleType :: TableFormDef -> Boolean
    tableFormDefIsForRoleType (TableFormDef {table, form}) = case table of
      TableDef {perspective} -> case perspective of 
        Nothing -> false 
        Just {roleType} -> case roleType of
          Just rt -> rt == roletype2string objectRoleType
          Nothing -> false

newtype SerialisedTableForm = SerialisedTableForm String
derive instance Newtype SerialisedTableForm _

constructDefaultTableForm :: RoleInstance -> RoleType -> RoleType -> ContextInstance -> MonadPerspectivesQuery TableFormDef
constructDefaultTableForm userRoleInstance userRoleType objectRoleType cid = do
  -- Find the perspective of the user in the context on the object.
  (perspective :: SerialisedPerspective') <- perspectiveForContextAndUser' userRoleInstance userRoleType objectRoleType cid
  let tableForm = TableFormDef
        { table: TableDef
            { title: Nothing
            , perspective: Just perspective
            , perspectiveId: ""
            , propertyVerbs: Nothing
            , roleVerbs: Nothing
            , userRole: userRoleType
            }
        , form: FormDef
            { title: Nothing
            , perspective: Just perspective
            , perspectiveId: ""
            , propertyVerbs: Nothing
            , roleVerbs: Nothing
            , userRole: userRoleType
            }
        }
  pure tableForm
