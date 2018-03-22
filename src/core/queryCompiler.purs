module Perspectives.QueryCompiler where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Trans.Class (lift)
import Data.Array (foldl, unsnoc, head)
import Data.Maybe (Maybe, fromJust)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, RolName, PropertyName)
import Perspectives.Identifiers (LocalName, deconstructLocalNameFromDomeinURI)
import Perspectives.PerspectivesState (MonadPerspectives)
import Perspectives.Property (ObjectsGetter, getContextType, getExternalProperty, getInternalProperty, getRolByLocalName)
import Perspectives.PropertyComposition (compose)
import Perspectives.QueryCombinators (closure, closure', concat, constant, contains, filter, lastElement, notEmpty, rolesOf, toBoolean)
import Perspectives.TripleAdministration (MonadPerspectivesQuery, NamedFunction(..), Triple(..), TripleGetter)
import Perspectives.TripleGetter (NamedTripleGetter, constructExternalPropertyGetter, constructExternalPropertyLookup, constructInternalPropertyGetter, constructInternalPropertyLookup, constructInverseRolGetter, constructRolGetter, constructRolLookup, constructRolPropertyGetter, constructRolPropertyLookup, constructTripleGetterFromObjectsGetter, constructTripleGetterFromObjectsGetter', putQueryVariable, readQueryVariable)
import Perspectives.Utilities (onNothing, onNothing')
import Prelude (bind, const, discard, pure, ($), (<$>), (<*>), (<<<), (<>), (>=>), (>>=))

-- | From a qualified name for a Rol, construct a function that computes the instances of that Rol for a given context.
-- | The Rol may be defined as computed.
rolQuery  :: forall e.
  RolName ->
  MonadPerspectivesQuery (AjaxAvarCache e) (NamedFunction (TripleGetter e))
rolQuery rn = do
  -- Is the type of rolType or one of its ancestors q:Query?
  (isAQuery :: Boolean) <- (toBoolean (contains "model:QueryAst$Query" (closure contextType)) rn)
  if isAQuery
    then do
      (NamedFunction nameOfq q) <- constructQueryFunction rn
      pure $ NamedFunction "saveInitialContext"
        \cid -> do
          putQueryVariable "#context" [cid]
          t@(Triple {object : values}) <- q cid
          pure t
    else pure $ constructRolLookup $ localName rn

  where
    contextType :: NamedTripleGetter e
    contextType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getContextType

    localName :: RolName -> LocalName
    localName qn = unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI qn)

-- | From a qualified name for a Property, construct a function that computes the values of that Property for a given rol.
-- | The Property may be defined as computed.
propertyQuery  :: forall e.
  PropertyName ->
  MonadPerspectivesQuery (AjaxAvarCache e) (NamedFunction (TripleGetter e))
propertyQuery pn = do
  -- Is the type of propertyType or one of its ancestors q:Query?
  (isAQuery :: Boolean) <- (toBoolean (contains "model:QueryAst$Query" (closure contextType)) pn)
  if isAQuery
    then do
      (NamedFunction nameOfq q) <- constructQueryFunction pn
      pure $ NamedFunction "saveInitialRol"
        \rid -> do
          putQueryVariable "#rol" [rid]
          t@(Triple {object : values}) <- q rid
          pure t
    else pure $ constructRolPropertyLookup $ localName pn

  where
    contextType :: NamedTripleGetter e
    contextType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getContextType

    localName :: RolName -> LocalName
    localName qn = unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI qn)

-- | From the id of a context that is a description of a Query, construct a function that computes the value of that
-- | query from the id of an entity.
-- TODO: voeg state toe waarin bijgehouden wordt welke variabelen al gedefinieerd zijn, zodat je kunt stoppen als vooruit verwezen wordt.
constructQueryFunction :: forall e.
  ContextID ->
  MonadPerspectivesQuery (AjaxAvarCache e) (NamedFunction (TripleGetter e))
constructQueryFunction typeDescriptionID = do
  pspType <- lift $ onNothing (errorMessage "no type found" "")
    (firstOnly getContextType typeDescriptionID)
  case pspType of
    "model:QueryAst$constructExternalPropertyGetter" ->
      applyPropertyConstructor constructExternalPropertyGetter pspType
    "model:QueryAst$constructExternalPropertyLookup" ->
      constructExternalPropertyLookup <$> onNothing' (errorMessage "no propertyName" pspType) (deconstructLocalNameFromDomeinURI typeDescriptionID)
    "model:QueryAst$constructInternalPropertyGetter" ->
      applyPropertyConstructor constructInternalPropertyGetter pspType
    "model:QueryAst$constructInternalPropertyLookup" ->
      constructInternalPropertyLookup <$> onNothing' (errorMessage "no propertyName" pspType) (deconstructLocalNameFromDomeinURI typeDescriptionID)
    "model:QueryAst$constructRolPropertyGetter" ->
      applyPropertyConstructor constructRolPropertyGetter pspType
    "model:QueryAst$constructRolPropertyLookup" ->
      constructRolPropertyLookup <$> onNothing' (errorMessage "no propertyName" pspType) (deconstructLocalNameFromDomeinURI typeDescriptionID)
    "model:QueryAst$getRol" -> do
      rolName <- lift $ onNothing (errorMessage "no rolName" pspType) (firstOnly (getRolByLocalName "rol") typeDescriptionID)
      rolQuery rolName
    -- Superfluous: can always be replaced by "model:QueryAst$getRol".
    "model:QueryAst$constructRolLookup" ->
      constructRolLookup <$> (onNothing' (errorMessage "no rolName" pspType) (deconstructLocalNameFromDomeinURI typeDescriptionID))
    -- Superfluous: can always be replaced by "model:QueryAst$constructRolLookup" and by "model:QueryAst$getRol".
    "model:QueryAst$constructRolGetter" ->
      lift $ constructRolGetter <$> onNothing (errorMessage "no rolName" pspType) (firstOnly (getRolByLocalName "rol") typeDescriptionID)
    "model:QueryAst$constructInverseRolGetter" ->
      lift $ constructInverseRolGetter <$> (onNothing (errorMessage "no rolName" pspType) (firstOnly (getRolByLocalName "rol") pspType))
    "model:QueryAst$rolesOf" ->
      lift $ rolesOf <$> (onNothing (errorMessage "no context" pspType) (firstOnly (getRolByLocalName "context") pspType))
    "model:QueryAst$notEmpty" -> applyUnaryCombinator notEmpty pspType
    "model:QueryAst$closure" -> applyUnaryCombinator closure pspType
    "model:QueryAst$closure'" -> applyUnaryCombinator closure' pspType
    "model:QueryAst$lastElement'" -> applyUnaryCombinator lastElement pspType
    "model:QueryAst$compose" -> applyBinaryCombinator compose pspType
    "model:QueryAst$concat" -> applyBinaryCombinator concat pspType
    "model:QueryAst$filter" -> do
      criteriumId <- lift $ onNothing (errorMessage "no criterium" pspType)
        (firstOnly (getRolByLocalName "criterium") typeDescriptionID)
      candidateId <- lift $ onNothing (errorMessage "no candidates" pspType)
        (firstOnly (getRolByLocalName "criterium") typeDescriptionID)
      filter <$> constructQueryFunction criteriumId <*> constructQueryFunction candidateId
    "model:QueryAst$Constant" -> do
      lift $ constant <$> onNothing (errorMessage "no constant value provided" pspType) (firstOnly (getExternalProperty "model:QueryAst$Constant$value") typeDescriptionID)
    "model:QueryAst$Variable" -> do
      variableName <- lift $ onNothing (errorMessage "no variable name found" pspType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      pure $ constructTripleGetterFromObjectsGetter' ("model:QueryAst$Variable_" <> variableName) (const $ readQueryVariable variableName)
    "model:QueryAst$setVariable" -> do
      variableName <- lift $ onNothing (errorMessage "no variable name found" pspType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      valueDescriptionID <- lift $ onNothing (errorMessage "no value found" pspType)
        (firstOnly (getRolByLocalName "value") typeDescriptionID)
      valueQuery@(NamedFunction nameOfq q) <- constructQueryFunction valueDescriptionID
      predicateName <- pure ("set_" <> variableName <> "_" <> nameOfq)
      pure $ NamedFunction predicateName
        \id -> do
          (Triple tf@{object : values}) <- q id
          putQueryVariable variableName values
          pure $ Triple tf {predicate = predicateName}

    -- Any other argument will be passed as is, thus implementing that we can create arbitrary contexts.
    _ -> pure $ constant typeDescriptionID
  where
    applyPropertyConstructor :: (PropertyName -> NamedTripleGetter e)
      -> ID
      -> MonadPerspectivesQuery (AjaxAvarCache e) (NamedTripleGetter e)
    applyPropertyConstructor f pspType = do
      id <- lift $ onNothing (errorMessage "no property found" pspType)
        (firstOnly (getRolByLocalName "property") typeDescriptionID)
      pure $ f id

    applyUnaryCombinator :: (NamedTripleGetter e -> NamedTripleGetter e )
      -> ID
      -> MonadPerspectivesQuery (AjaxAvarCache e) (NamedTripleGetter e)
    applyUnaryCombinator c pspType = do
      queryId <- lift $ onNothing (errorMessage "no query found" pspType)
        (firstOnly (getRolByLocalName "query") typeDescriptionID)
      ((constructQueryFunction queryId) >>= pure <<< c)

    applyBinaryCombinator :: (NamedTripleGetter e -> NamedTripleGetter e -> NamedTripleGetter e)
      -> ID
      -> MonadPerspectivesQuery (AjaxAvarCache e) (NamedTripleGetter e)
    applyBinaryCombinator c pspType = do
      (operandIds :: Array ID) <- lift $ getRolByLocalName "operand" typeDescriptionID
      operands <- traverse constructQueryFunction operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" pspType) (unsnoc operands)
      pure $ foldl c last init

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructEffectStatement: " <> s <> " for: " <> t <> " " <> typeDescriptionID)

    firstOnly :: ObjectsGetter e -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String))
    firstOnly g = g >=> (pure <<< head)
