{-
  TODO.
  Nadat de Arc parser is gemaakt, kunnen we deze triple getters weer inhoud geven.
  Tot die tijd heb ik de import ervan in Main uitgecommentarieerd.
  BasicActionFunctions (die ook importeert) is ook tijdelijk uitgecommentarieerd in Actions.
-}
module Perspectives.ComputedTripleGetters where

import Prelude

import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, null)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Perspectives.CollectDomeinFile (domeinFileFromContext)
import Perspectives.ContextRoleParser (ParseRoot(..), parseAndCache)
import Perspectives.CoreTypes (MonadPerspectivesQuery, MonadPerspectives, (@@>>))
import Perspectives.DomeinCache (documentNamesInDatabase)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (buitenRol, isQualifiedWithDomein)
import Perspectives.PerspectivesTypes (PropertyDef(..))
import Perspectives.QueryCache (queryCacheInsert)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery ((##=), (##>>))
import Perspectives.StringTripleGetterConstructors (StringTypedTripleGetter, getInternalProperty)
import Perspectives.TripleGetterComposition (followedBy)
import Perspectives.TripleGetterFromObjectGetter (constructExternalPropertySearch, constructTripleGetterWithArbitrarySupport)
import Perspectives.TypeDefChecker (checkDomeinFile)

-- | This TypedTripleGetter computes a list of the buitenrol-IDs of all models that are available to this system.
modellenM :: StringTypedTripleGetter
modellenM = constructTripleGetterWithArbitrarySupport
  "model:Perspectives$PerspectivesSysteem$modellen" getListOfModels (constructExternalPropertySearch (PropertyDef "model:Perspectives$TrustedCluster$buitenRolBeschrijving$modelOphaalTeller") `followedBy` unwrap)
  where
    getListOfModels :: ID -> MonadPerspectivesQuery (Array String)
    getListOfModels id = lift $ lift $ catchError ((documentNamesInDatabase "perspect_models") >>= pure <<< map buitenRol) \_ -> pure []

-- | Given the ID of a context of type model:CrlText$Text, computes an array of strings that are either the identifier
-- | of the model, or the identifiers of the BuitenRollen of the userdata, or error messages from the attempt to parse
-- | the file. Notice that the parseresult is stored automatically by the parser.
parserMessagesM :: StringTypedTripleGetter
parserMessagesM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$parserMessages" parseSourceText (getInternalProperty "model:CrlText$Text$binnenRolBeschrijving$sourceText")
  where

    parseSourceText :: ID -> MonadPerspectivesQuery (Array String)
    parseSourceText textId = do
      sourceText <- (textId @@>> getInternalProperty "model:CrlText$Text$binnenRolBeschrijving$sourceText")
      -- TODO. Here we connect the new ARC parser.
      parseResult <- lift $ parseAndCache sourceText
      case parseResult of
        (Right (Tuple parseRoot domeinFile)) -> case parseRoot of
          (RootContext rootId) -> pure [rootId]
          (UserData buitenRollen) -> pure buitenRollen
        (Left e) -> pure [(show e)]

syntacticStateM :: StringTypedTripleGetter
syntacticStateM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$syntacticState" f parserMessagesM
  where
    f :: ID -> MonadPerspectivesQuery (Array String)
    f textId = do
      m <- lift (textId ##= parserMessagesM)
      case head m of
        (Just mid) | isQualifiedWithDomein mid -> pure ["true"]
        otherwise -> pure ["false"]

typeCheckerMessagesM :: StringTypedTripleGetter
typeCheckerMessagesM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$typeCheckerMessages" checkModel_ (getInternalProperty "model:CrlText$Text$binnenRolBeschrijving$sourceText")
  where
    checkModel_ :: ID -> MonadPerspectivesQuery (Array String)
    checkModel_ textId = do
      syntacticState <- lift (textId ##>> syntacticStateM)
      case syntacticState of
        "false" -> pure ["The syntactic state does not allow type checking."]
        otherwise -> do
          contextId <- lift (textId ##>> parserMessagesM)
          -- As the syntactic state is ok, we can assume safely its root context exists in the cache.
          ctxt <- lift $ getPerspectEntiteit contextId
          df <- lift $ domeinFileFromContext ctxt
          um <- checkDomeinFile df
          if null um
            then pure []
            else pure (map show um)

semanticStateM :: StringTypedTripleGetter
semanticStateM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$semanticState" f parserMessagesM
  where
    f :: ID -> MonadPerspectivesQuery (Array String)
    f textId = do
      m <- lift (textId ##= typeCheckerMessagesM)
      if null m
        then pure ["true"]
        else pure ["false"]

computedTripleGetters :: Array (Tuple String (StringTypedTripleGetter))
computedTripleGetters = [
  Tuple "modellenM" modellenM,
  Tuple "parserMessagesM" parserMessagesM,
  Tuple "syntacticStateM" syntacticStateM,
  Tuple "typeCheckerMessagesM" typeCheckerMessagesM,
  Tuple "semanticStateM" semanticStateM
]

addComputedTripleGetters :: MonadPerspectives  Unit
addComputedTripleGetters = for_ computedTripleGetters \(Tuple n f) -> queryCacheInsert n f
