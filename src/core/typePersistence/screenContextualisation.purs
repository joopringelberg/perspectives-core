module Perspectives.TypePersistence.ScreenContextualisation where

import Prelude

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes, elemIndex, filter, filterA, head, null)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQuery, (###=))
import Perspectives.DependencyTracking.Array.Trans (runArrayT)
import Perspectives.Identifiers (typeUri2ModelUri_)
import Perspectives.Instances.ObjectGetters (getActiveRoleStates, getActiveStates)
import Perspectives.ModelTranslation (translationOf)
import Perspectives.Query.UnsafeCompiler (getRoleInstances)
import Perspectives.Representation.Class.Role (perspectivesOfRoleType)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.Perspective (Perspective(..), StateSpec(..))
import Perspectives.Representation.ScreenDefinition (ChatDef(..), ColumnDef(..), FormDef(..), MarkDownDef(..), RowDef(..), ScreenDefinition(..), ScreenElementDef(..), TabDef(..), TableDef(..), TableFormDef(..), WidgetCommonFieldsDef)
import Perspectives.Representation.TypeIdentifiers (ContextType, RoleType(..))
import Perspectives.TypePersistence.PerspectiveSerialisation (serialisePerspective)
import Perspectives.Types.ObjectGetters (allEnumeratedRoles, aspectsOfRole)

type Context = {userRoleInstance :: RoleInstance, contextType :: ContextType, contextInstance :: ContextInstance }
type InContext = ReaderT Context MonadPerspectivesQuery

contextualiseScreen :: ScreenDefinition -> InContext (Maybe ScreenDefinition)
contextualiseScreen (ScreenDefinition{title, tabs, rows, columns}) = do
  tabs' <- emptyArrayToNothing <<< map catMaybes <$> (for tabs (traverse contextualiseTab))
  rows' <- emptyArrayToNothing <<< map catMaybes <$> (for rows (traverse contextualiseScreenElementDef))
  columns' <- (emptyArrayToNothing <<< map catMaybes) <$> (for columns (traverse contextualiseScreenElementDef))
  pure $ Just $ ScreenDefinition{title, tabs: tabs', rows: rows', columns: columns', whoWhatWhereScreen: Nothing}

emptyArrayToNothing :: forall a. Maybe (Array a) -> Maybe (Array a)
emptyArrayToNothing marr = case marr of 
  Nothing -> Nothing
  Just arr -> if null arr 
    then Nothing
    else Just arr

contextualiseTab :: TabDef -> InContext (Maybe TabDef)
contextualiseTab (TabDef {title, isDefault, elements}) = do 
  elements' <- catMaybes <$> (for elements contextualiseScreenElementDef)
  {contextType} <- ask
  (translatedTitle :: String) <- lift2InContext (translationOf (unsafePartial typeUri2ModelUri_ $ unwrap contextType) title) 
  if null elements'
    then pure Nothing 
    else pure $ Just $ TabDef {title: translatedTitle, isDefault, elements: elements'}

contextualiseScreenElementDef :: ScreenElementDef -> InContext (Maybe ScreenElementDef)
contextualiseScreenElementDef (RowElementD e) = map RowElementD <$> contextualiseRowDef e
contextualiseScreenElementDef (ColumnElementD e) = map ColumnElementD <$> contextualiseColumnDef e
contextualiseScreenElementDef (TableElementD e) = map TableElementD <$> contextualiseTableDef e
contextualiseScreenElementDef (FormElementD e) = map FormElementD <$> contextualiseFormDef e
contextualiseScreenElementDef (MarkDownElementD e) = map MarkDownElementD <$> contextualiseMarkDownDef e
contextualiseScreenElementDef (ChatElementD c) = map ChatElementD <$> contextualiseChatDef c 

contextualiseRowDef :: RowDef -> InContext (Maybe RowDef)
contextualiseRowDef (RowDef elements) = do
  elements' <- catMaybes <$> (for elements contextualiseScreenElementDef)
  if null elements'
    then pure Nothing
    else pure $ Just $ RowDef elements'

contextualiseColumnDef :: ColumnDef -> InContext (Maybe ColumnDef)
contextualiseColumnDef (ColumnDef elements) = do
  elements' <- catMaybes <$> (for elements contextualiseScreenElementDef)
  if null elements'
    then pure Nothing 
    else pure $ Just $ ColumnDef elements'

contextualiseTableDef :: TableDef -> InContext (Maybe TableDef)
contextualiseTableDef (TableDef widgetFields) = map TableDef <$> contextualiseWidgetCommonFields widgetFields

contextualiseFormDef :: FormDef -> InContext (Maybe FormDef)
contextualiseFormDef (FormDef widgetFields) = map FormDef <$> contextualiseWidgetCommonFields widgetFields

contextualiseMarkDownDef :: MarkDownDef -> InContext (Maybe MarkDownDef)
contextualiseMarkDownDef md = case md of 
  MarkDownPerspectiveDef {widgetFields, conditionProperty} -> do 
    mwidgetFields <- contextualiseWidgetCommonFields widgetFields
    case mwidgetFields of 
      Just widgetFields' -> pure $ Just $ MarkDownPerspectiveDef {widgetFields: widgetFields', conditionProperty}
      Nothing -> pure Nothing
  MarkDownConstantDef r@{text, domain} -> do 
    translatedText <- lift2InContext $ translationOf domain text
    pure $ Just $ MarkDownConstantDef r {text = translatedText}
  _ -> pure $ Just md

contextualiseChatDef :: ChatDef -> InContext (Maybe ChatDef)
contextualiseChatDef (ChatDef r@{chatRole}) = do 
  {contextInstance} <- ask
  chatRoleInstance <- lift $ getRoleInstances chatRole contextInstance
  pure $ Just $ ChatDef r {chatInstance = Just chatRoleInstance}

contextualiseWidgetCommonFields :: WidgetCommonFieldsDef -> InContext (Maybe WidgetCommonFieldsDef)
contextualiseWidgetCommonFields wc@{title, perspectiveId, propertyVerbs, roleVerbs, userRole} = do
  {contextInstance, userRoleInstance, contextType} <- ask
  contextStates <- lift $ lift (map ContextState <$> (runArrayT $ getActiveStates contextInstance))
  subjectStates <- lift $ lift (map SubjectState <$> (runArrayT $ getActiveRoleStates userRoleInstance))
  allPerspectives <- lift2InContext $ perspectivesOfRoleType userRole
  perspective <- pure $ unsafePartial fromJust $ head (filter
    (\(Perspective{id}) -> id == perspectiveId)
    allPerspectives)
  mperspective <- contextualisePerspective perspective
  (translatedTitle :: Maybe String) <- lift2InContext $ traverse (translationOf (unsafePartial typeUri2ModelUri_$ unwrap contextType)) title
  for mperspective (serialise translatedTitle contextStates subjectStates)
  where 
    serialise :: Maybe String -> Array StateSpec -> Array StateSpec -> Perspective -> InContext WidgetCommonFieldsDef
    serialise translatedTitle contextStates subjectStates perspective = do 
      {contextInstance} <- ask
      serialisedPerspective <- lift $ lift $ serialisePerspective contextStates subjectStates contextInstance userRole propertyVerbs roleVerbs perspective
      pure $ wc {perspective = Just serialisedPerspective, title = translatedTitle}

contextualisePerspective :: Perspective -> InContext (Maybe Perspective)
contextualisePerspective p@(Perspective pr) = if pr.isEnumerated 
  then do 
    -- since the perspective is enumerated, we know there is but a single, EnumeratedRoleType in `roleTypes`.
    roleType <- pure (unsafePartial fromJust $ head pr.roleTypes) >>= unsafePartial case _ of ENR roleType -> pure roleType
    {contextType} <- ask
    allRoles <- lift2InContext (contextType ###= allEnumeratedRoles)
    if isJust $ elemIndex roleType allRoles
      -- The context has the roleType, probably as an aspect role.
      then pure $ Just p 
      -- find a role in contextType that has roleType as aspect
      else do 
        rolesWithAspect <- filterA 
          (\erole -> do
            aspects <- lift2InContext (erole ###= aspectsOfRole)
            pure $ isJust $ elemIndex roleType aspects)
          allRoles
        case head rolesWithAspect of 
          -- No role has the type we're looking for. This perspective should not be used in the screen.
          Nothing -> pure Nothing
          -- This role has roleType as aspect. Contextualise the perspective.
          Just roleWithAspect -> pure $ Just $ Perspective pr { roleTypes = [ENR roleWithAspect], displayName = show roleWithAspect }
  -- A calculated perspective may work. We cannot say.
  else pure $ Just p

lift2InContext :: forall a. MonadPerspectives a -> InContext a
lift2InContext = lift <<< lift <<< lift

contextualiseTableFormDef :: TableFormDef -> InContext (Maybe TableFormDef)
contextualiseTableFormDef (TableFormDef {table, form}) = do
  table' <- contextualiseTableDef table
  form' <- contextualiseFormDef form
  pure $ case table' of 
    Nothing -> Nothing
    Just table'' -> case form' of 
      Nothing -> Nothing
      Just form'' -> Just $ TableFormDef {table: table'', form: form''}
