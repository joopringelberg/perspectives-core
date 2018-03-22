module Perspectives.EffectStatementCompiler where

import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Perspectives.Deltas (MonadTransactie, addProperty, addRol, removeProperty, removeRol, setProperty, setRol)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, Subject)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (ObjectsGetter, getContextType, getExternalProperty, getInternalProperty, getProperty, getRol, getRolByLocalName)
import Perspectives.TripleAdministration (MonadPerspectivesQuery)
import Perspectives.TripleGetter (putQueryVariable, readQueryVariable, runTripleGetter)
import Perspectives.Utilities (onNothing)
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), (>=>), discard)

type Statement e = ContextID -> MonadTransactie e Unit

type EffectExpression e = Subject -> MonadPerspectivesQuery e (Array String)

constructEffectExpressie :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (EffectExpression (AjaxAvarCache e))
constructEffectExpressie typeDescriptionID = do
  pspType <- onNothing (errorMessage "no type found" "")
    (firstOnly getContextType typeDescriptionID)
  case pspType of
    "model:QueryAst$Constant" -> do
      arrWithConst <- getExternalProperty "model:QueryAst$Constant$value" typeDescriptionID
      pure $ const $ pure arrWithConst
    "model:QueryAst$Variable" -> do
      variableName <- onNothing (errorMessage "no variable name found" pspType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      pure \cid -> readQueryVariable variableName
    "model:QueryAst$setVariable" -> do
      variableName <- onNothing (errorMessage "no variable name found" pspType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      valueDescriptionID <- onNothing (errorMessage "no value found" pspType)
        (firstOnly (getRolByLocalName "value") typeDescriptionID)
      valueQuery <- constructEffectExpressie valueDescriptionID
      pure \cid -> do
        (values :: Array ID) <- lift $ runTripleGetter cid valueQuery
        putQueryVariable variableName values
        pure []

    -- Any other argument will be passed as is, thus implementing that we can create arbitrary contexts.
    _ -> pure $ const $ pure [typeDescriptionID]
  where
    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructEffectStatement: " <> s <> " for: " <> t <> " " <> typeDescriptionID)


constructEffectStatement :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (Statement e)
constructEffectStatement typeDescriptionID = do
  pspType <- onNothing (errorMessage "no type found" "")
    (firstOnly getContextType typeDescriptionID)
  case pspType of
    "model:QueryAst$assignToRol" -> do
      operationType <- onNothing (errorMessage "no operation type found" pspType)
        (firstOnly (getInternalProperty "model:QueryAst$assignToRol$operation") typeDescriptionID)
      operation <- case operationType of
        "add" -> pure addRol
        "remove" -> pure removeRol
        _ -> pure setRol
      rolName <- onNothing (errorMessage "no rol found" pspType)
        (firstOnly (getRolByLocalName "rol") typeDescriptionID)
      valueDescriptionID <- onNothing (errorMessage "no value found" pspType)
        (firstOnly (getRolByLocalName "value") typeDescriptionID)
      valueQuery <- constructEffectExpressie valueDescriptionID
      pure (\cid -> do
                      (values :: Array ID) <- lift $ runTripleGetter cid valueQuery
                      for_ values \rolId -> operation cid rolName rolId)
    "model:QueryAst$assignToProperty" -> do
      operationType <- onNothing (errorMessage "no operation type found" pspType)
        (firstOnly (getInternalProperty "model:QueryAst$assignToProperty$operation") typeDescriptionID)
      operation <- case operationType of
        "add" -> pure addProperty
        "remove" -> pure removeProperty
        _ -> pure setProperty
      rolName <- onNothing (errorMessage "no rol found" pspType)
        (firstOnly (getRolByLocalName "rol") typeDescriptionID)
      propertyName <- onNothing (errorMessage "no property found" pspType)
        (firstOnly (getRolByLocalName "property") typeDescriptionID)
      valueDescriptionID <- onNothing (errorMessage "no value found" pspType)
        (firstOnly (getRolByLocalName "value") typeDescriptionID)
      valueQuery <- constructEffectExpressie valueDescriptionID
      pure \cid -> do
        rolId <- onNothing (error $ "Missing " <> rolName <> " in " <> cid)
          (lift (firstOnly (getRol rolName) cid))
        (values :: Array ID) <- lift $ runTripleGetter cid valueQuery
        for_ values \val -> operation rolId propertyName val

    _ -> pure emptyStatement
  where
    emptyStatement :: (Statement e)
    emptyStatement = const $ pure unit

    emptyResultExpression :: (EffectExpression (AjaxAvarCache e))
    emptyResultExpression = const $ pure []

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructEffectStatement: " <> s <> " for: " <> t <> " " <> typeDescriptionID)

firstOnly :: forall e. ObjectsGetter e -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String))
firstOnly g = g >=> (pure <<< head)
