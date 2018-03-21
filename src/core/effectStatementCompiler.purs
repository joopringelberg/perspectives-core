module Perspectives.EffectStatementCompiler where

import Control.Monad.Eff.Exception (error, Error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), maybe)
import Perspectives.Deltas (MonadTransactie, addProperty, addRol)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, Subject)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (ObjectsGetter, getContextType, getExternalProperty, getInternalProperty, getProperty, getRol, getRolByLocalName)
import Perspectives.TripleAdministration (MonadPerspectivesQuery)
import Perspectives.TripleGetter (readQueryVariable, runTripleGetter)
import Perspectives.Utilities (onNothing)
import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<>), (>=>), (>>=))

type Statement e = ContextID -> MonadTransactie e Unit

type EffectExpression e = Subject -> MonadPerspectivesQuery e (Array String)

constructEffectExpressie :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (EffectExpression (AjaxAvarCache e))
constructEffectExpressie typeDescriptionID = do
  (pspType :: Array ID) <- getContextType typeDescriptionID
  case head pspType of
    Nothing -> emptyResultExpression
    (Just "model:Effect$Constant") -> do
      arrWithConst <- getExternalProperty "model:Effect$Constant$value" typeDescriptionID
      pure $ const $ pure arrWithConst
    (Just "model:Effect$Effect$variable") -> do
      mVar <- getProperty "model:Effect$Variable$name" typeDescriptionID
      case head mVar of
        Nothing -> emptyResultExpression
        Just var -> pure $ \_ -> readQueryVariable var
    (Just _) -> emptyResultExpression
  where
    emptyResultExpression :: MonadPerspectives (AjaxAvarCache e) (EffectExpression (AjaxAvarCache e))
    emptyResultExpression = pure $ const $ pure []

constructEffectStatement :: forall e. ContextID -> MonadPerspectives (AjaxAvarCache e) (Statement e)
constructEffectStatement typeDescriptionID = do
  pspType <- onNothing (errorMessage "no type found" "")
    (firstOnly getContextType typeDescriptionID)
  case pspType of
    "model:QueryAst$addRol" -> do
      rolName <- onNothing (errorMessage "no rol found" pspType)
        (firstOnly (getRolByLocalName "rol") typeDescriptionID)
      valueDescriptionID <- onNothing (errorMessage "no value found" pspType)
        (firstOnly (getRolByLocalName "value") typeDescriptionID)
      valueQuery <- constructEffectExpressie valueDescriptionID
      pure (\cid -> do
                      (values :: Array ID) <- lift $ runTripleGetter cid valueQuery
                      for_ values \rolId -> addRol cid rolName rolId)
    "model:QueryAst$addProperty" -> do
      rolName <- onNothing (errorMessage "no rol found" pspType)
        (firstOnly (getRolByLocalName "rol") typeDescriptionID)
      propertyName <- onNothing (errorMessage "no property found" pspType)
        (firstOnly (getRolByLocalName "property") typeDescriptionID)
      mValueDescriptionID <- firstOnly (getRolByLocalName "value") typeDescriptionID
      case mValueDescriptionID of
        Nothing -> do
          constantValue <- onNothing (errorMessage "no value found" pspType)
            (firstOnly (getInternalProperty "model:QueryAst$addProperty$constantValue") typeDescriptionID)
          pure \cid -> do
            rolId <- onNothing (error $ "Missing " <> rolName <> " in " <> cid)
                          (lift (firstOnly (getRol rolName) cid))
            addProperty rolId propertyName constantValue
        (Just valueDescriptionID) -> do
          valueQuery <- constructEffectExpressie valueDescriptionID
          pure \cid -> do
            rolId <- onNothing (error $ "Missing " <> rolName <> " in " <> cid)
              (lift (firstOnly (getRol rolName) cid))
            (values :: Array ID) <- lift $ runTripleGetter cid valueQuery
            for_ values \val -> addProperty rolId propertyName val

    _ -> pure emptyStatement
  where
    emptyStatement :: (Statement e)
    emptyStatement = const $ pure unit

    emptyResultExpression :: (EffectExpression (AjaxAvarCache e))
    emptyResultExpression = const $ pure []

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructEffectStatement: " <> s <> " for: " <> t <> " " <> typeDescriptionID)

    firstOnly :: ObjectsGetter e -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String))
    firstOnly g = g >=> (pure <<< head)
