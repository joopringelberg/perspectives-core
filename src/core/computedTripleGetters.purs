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
import Data.Tuple (Tuple(..))
import Perspectives.Checking.PerspectivesTypeChecker (checkDomeinFile)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (MonadPerspectivesQuery, MonadPerspectives, (@@>>), StringTypedTripleGetter)
import Perspectives.DomeinCache (documentNamesInDatabase, retrieveDomeinFile)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Identifiers (buitenRol, isQualifiedWithDomein)
import Perspectives.ObjectGetterConstructors (searchExternalProperty)
import Perspectives.QueryCache (queryCacheInsert)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..))
import Perspectives.RunMonadPerspectivesQuery ((##=), (##>>))
import Perspectives.TripleGetters.TrackedAs (constructTripleGetterWithArbitrarySupport, trackedAs)

-- | This TypedTripleGetter computes a list of the buitenrol-IDs of all models that are available to this system.
modellenM :: StringTypedTripleGetter
modellenM = constructTripleGetterWithArbitrarySupport "model:Perspectives$PerspectivesSysteem$modellen" getListOfModels ophaalTeller
  where
    getListOfModels :: ID -> MonadPerspectivesQuery (Array String)
    getListOfModels id = lift $ lift $ catchError ((documentNamesInDatabase "perspect_models") >>= pure <<< map buitenRol) \_ -> pure []

    ophaalTeller :: StringTypedTripleGetter
    ophaalTeller = searchExternalProperty (EnumeratedPropertyType ophaalTellerName) `trackedAs` ophaalTellerName

    ophaalTellerName :: String
    ophaalTellerName = "model:Perspectives$TrustedCluster$buitenRolBeschrijving$modelOphaalTeller"

-- | Given the ID of a context of type model:CrlText$Text, computes an array of strings that are either the identifier
-- | of the model, or the identifiers of the BuitenRollen of the userdata, or error messages from the attempt to parse
-- | the file. Notice that the parseresult is stored automatically by the parser.
parserMessagesM :: StringTypedTripleGetter
parserMessagesM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$parserMessages" parseSourceText sourceText
  where

    parseSourceText :: ID -> MonadPerspectivesQuery (Array String)
    parseSourceText textId = do
      txt <- textId @@>> sourceText
      -- TODO. Here we connect the new ARC parser.
      parseResult <- lift $ parseAndCache txt
      case parseResult of
        (Right buitenRollen) -> pure buitenRollen
        (Left e) -> pure [(show e)]

sourceText :: StringTypedTripleGetter
sourceText = searchExternalProperty (EnumeratedPropertyType sourceTextName) `trackedAs` sourceTextName

sourceTextName :: String
sourceTextName = "model:CrlText$Text$binnenRolBeschrijving$sourceText"

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
  "model:CrlText$Text$binnenRolBeschrijving$typeCheckerMessages" checkModel_ sourceText
  where
    checkModel_ :: ID -> MonadPerspectivesQuery (Array String)
    checkModel_ textId = do
      syntacticState <- lift (textId ##>> syntacticStateM)
      case syntacticState of
        "false" -> pure ["The syntactic state does not allow type checking."]
        otherwise -> do
          df <- lift $ retrieveDomeinFile textId
          um <- lift $ checkDomeinFile df
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
