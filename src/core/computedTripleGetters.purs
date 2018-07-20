module Perspectives.ComputedTripleGetters where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Perspectives.CollectDomeinFile (domeinFileFromContext)
import Perspectives.ContextRoleParser (ParseRoot(..), parseAndCache)
import Perspectives.CoreTypes (MonadPerspectivesQuery, TypedTripleGetter, MonadPerspectives, (%%>>))
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Identifiers (isQualifiedWithDomein)
import Perspectives.ObjectGetterConstructors (getInternalProperty)
import Perspectives.QueryCache (queryCacheInsert)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery ((##=), (##>>))
import Perspectives.TripleGetterConstructors (constructExternalPropertyGetter, constructInternalPropertyGetter, constructTripleGetterWithArbitrarySupport)
import Perspectives.TypeDefChecker (checkContext, checkDomeinFile)

-- | This TypedTripleGetter computes a list of the IDs of all models that are available to this system.
modellenM :: forall e1. TypedTripleGetter e1
modellenM = constructTripleGetterWithArbitrarySupport
  "model:Systeem$Systeem$modellen" getListOfModels (constructExternalPropertyGetter "model:Systeem$TrustedCluster$buitenRolBeschrijving$modelOphaalTeller")

getListOfModels :: forall e. ID -> MonadPerspectivesQuery (ajax :: AJAX | e) (Array String)
getListOfModels id = lift $ lift $ catchError (documentNamesInDatabase "perspect_models") \_ -> pure []

parserMessagesM :: forall e. TypedTripleGetter e
parserMessagesM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$parserMessages" parseSourceText (constructInternalPropertyGetter "model:CrlText$Text$binnenRolBeschrijving$sourceText")

parseSourceText :: forall e. ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String)
parseSourceText textId = do
  sourceText <- lift (textId %%>> getInternalProperty "model:CrlText$Text$binnenRolBeschrijving$sourceText")
  parseResult <- lift $ parseAndCache sourceText
  case parseResult of
    (Right parseRoot) -> case parseRoot of
      (RootContext rootId) -> pure [rootId]
      (UserData buitenRollen) -> pure buitenRollen
    (Left e) -> pure [(show e)]

syntacticStateM :: forall e. TypedTripleGetter e
syntacticStateM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$syntacticState" f parserMessagesM
  where
    f :: ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String)
    f textId = do
      m <- lift (textId ##= parserMessagesM)
      case head m of
        (Just mid) | isQualifiedWithDomein mid -> pure ["true"]
        otherwise -> pure ["false"]

typeCheckerMessagesM :: forall e. TypedTripleGetter e
typeCheckerMessagesM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$typeCheckerMessages" checkModel_ (constructInternalPropertyGetter "model:CrlText$Text$binnenRolBeschrijving$sourceText")

checkModel_ :: forall e. ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String)
checkModel_ textId = do
  syntacticState <- lift (textId ##>> syntacticStateM)
  case syntacticState of
    "false" -> pure ["The syntactic state does not allow type checking."]
    otherwise -> do
      contextId <- lift (textId ##>> parserMessagesM)
      ctxt <- lift $ getPerspectEntiteit contextId
      df <- lift $ domeinFileFromContext ctxt
      um <- checkDomeinFile df
      if null um
        then pure []
        else pure (map show um)

semanticStateM :: forall e. TypedTripleGetter e
semanticStateM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$semanticState" f parserMessagesM
  where
    f :: ID -> MonadPerspectivesQuery (AjaxAvarCache e) (Array String)
    f textId = do
      m <- lift (textId ##= typeCheckerMessagesM)
      if null m
        then pure ["true"]
        else pure ["false"]

computedTripleGetters :: forall e. Array (Tuple String (TypedTripleGetter e))
computedTripleGetters = [
  Tuple "modellenM" modellenM,
  Tuple "parserMessagesM" parserMessagesM,
  Tuple "syntacticStateM" syntacticStateM,
  Tuple "typeCheckerMessagesM" typeCheckerMessagesM,
  Tuple "semanticStateM" semanticStateM
]

addComputedTripleGetters :: forall e. MonadPerspectives (gm :: GLOBALMAP | e) Unit
addComputedTripleGetters = for_ computedTripleGetters \(Tuple n f) -> queryCacheInsert n f
