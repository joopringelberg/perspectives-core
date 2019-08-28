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
import Perspectives.Checking.PerspectivesTypeChecker (checkDomeinFile)
import Perspectives.ContextRoleParser (parseAndCache)
import Perspectives.CoreTypes (MonadPerspectivesQuery, MonadPerspectives, (@@>>), StringTypedTripleGetter, type (**>))
import Perspectives.DomeinCache (documentNamesInDatabase, retrieveDomeinFile)
import Perspectives.Identifiers (buitenRol, isQualifiedWithDomein)
import Perspectives.ObjectGetterConstructors (searchExternalProperty)
import Perspectives.QueryCache (queryCacheInsert)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance(..), Value(..))
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType(..))
import Perspectives.RunMonadPerspectivesQuery ((##=), (##>>))
import Perspectives.TripleGetters.TrackedAs (constructTripleGetterWithArbitrarySupport, trackedAs)
import Unsafe.Coerce (unsafeCoerce)

-- | This TypedTripleGetter computes a list of the buitenrol-IDs of all models that are available to this system.
modellenM :: ContextInstance **> RoleInstance
modellenM = constructTripleGetterWithArbitrarySupport "model:Perspectives$PerspectivesSysteem$modellen" getListOfModels ophaalTeller
  where
    getListOfModels :: ContextInstance -> MonadPerspectivesQuery (Array RoleInstance)
    getListOfModels id = lift $ lift $ catchError ((documentNamesInDatabase "perspect_models") >>= pure <<< map (RoleInstance <<< buitenRol)) \_ -> pure []

    ophaalTeller :: ContextInstance **> Value
    ophaalTeller = searchExternalProperty (EnumeratedPropertyType ophaalTellerName) `trackedAs` ophaalTellerName

    ophaalTellerName :: String
    ophaalTellerName = "model:Perspectives$TrustedCluster$buitenRolBeschrijving$modelOphaalTeller"

-- | Given the ID of a context of type model:CrlText$Text, computes an array of strings that are either the identifier
-- | of the model, or the identifiers of the BuitenRollen of the userdata, or error messages from the attempt to parse
-- | the file. Notice that the parseresult is stored automatically by the parser.
parserMessagesM :: ContextInstance **> Value
parserMessagesM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$parserMessages" parseSourceText sourceText
  where

    parseSourceText :: ContextInstance -> MonadPerspectivesQuery (Array Value)
    parseSourceText textId = do
      txt <- textId @@>> sourceText
      -- TODO. Here we connect the new ARC parser.
      parseResult <- lift $ parseAndCache $ unwrap txt
      case parseResult of
        (Right buitenRollen) -> pure $ map (Value <<< unwrap) buitenRollen
        (Left e) -> pure [Value (show e)]

sourceText :: ContextInstance **> Value
sourceText = searchExternalProperty (EnumeratedPropertyType sourceTextName) `trackedAs` sourceTextName

sourceTextName :: String
sourceTextName = "model:CrlText$Text$binnenRolBeschrijving$sourceText"

syntacticStateM :: ContextInstance **> Value
syntacticStateM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$syntacticState" f parserMessagesM
  where
    f :: ContextInstance -> MonadPerspectivesQuery (Array Value)
    f textId = do
      m <- lift (textId ##= parserMessagesM)
      case head m of
        (Just mid) | isQualifiedWithDomein $ unwrap mid -> pure [Value "true"]
        otherwise -> pure [Value "false"]

typeCheckerMessagesM :: ContextInstance **> Value
typeCheckerMessagesM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$typeCheckerMessages" checkModel_ sourceText
  where
    checkModel_ :: ContextInstance -> MonadPerspectivesQuery (Array Value)
    checkModel_ textId = do
      syntacticState <- lift (textId ##>> syntacticStateM)
      case syntacticState of
        Value "false" -> pure [Value "The syntactic state does not allow type checking."]
        otherwise -> do
          df <- lift $ retrieveDomeinFile $ unwrap textId
          um <- lift $ checkDomeinFile df
          if null um
            then pure []
            else pure (map (Value <<< show) um)

semanticStateM :: ContextInstance **> Value
semanticStateM = constructTripleGetterWithArbitrarySupport
  "model:CrlText$Text$binnenRolBeschrijving$semanticState" f parserMessagesM
  where
    f :: ContextInstance -> MonadPerspectivesQuery (Array Value)
    f textId = do
      m <- lift (textId ##= typeCheckerMessagesM)
      if null m
        then pure [Value "true"]
        else pure [Value "false"]

computedTripleGetters :: Array (Tuple String (StringTypedTripleGetter))
computedTripleGetters = [
  Tuple "modellenM" $ unsafeCoerce modellenM,
  Tuple "parserMessagesM" $ unsafeCoerce parserMessagesM,
  Tuple "syntacticStateM" $ unsafeCoerce syntacticStateM,
  Tuple "typeCheckerMessagesM" $ unsafeCoerce typeCheckerMessagesM,
  Tuple "semanticStateM" $ unsafeCoerce semanticStateM
]

addComputedTripleGetters :: MonadPerspectives  Unit
addComputedTripleGetters = for_ computedTripleGetters \(Tuple n f) -> queryCacheInsert n f
