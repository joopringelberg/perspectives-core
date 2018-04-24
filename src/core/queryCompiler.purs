module Perspectives.QueryCompiler where

import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (elemIndex, foldl, head, unsnoc)
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (Domain, MonadPerspectives, MonadPerspectivesQuery, MonadPerspectivesQueryCompiler, ObjectsGetter, Range, Triple(..), TypedTripleGetter(..), getQueryStepDomain, getQueryVariableType, putQueryStepDomain, runMonadPerspectivesQueryCompiler, withQueryCompilerEnvironment, (##), tripleObjects, putQueryVariable, readQueryVariable)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolName, RolID)
import Perspectives.Identifiers (LocalName, deconstructLocalNameFromDomeinURI)
import Perspectives.Property (getContextType, getContextTypeF, getExternalProperty, getInternalProperty, getRol, getRolByLocalName, makeFunction)
import Perspectives.PropertyComposition (compose)
import Perspectives.QueryCombinators (closure, closure', concat, constant, contains, filter, lastElement, notEmpty, rolesOf, toBoolean)
import Perspectives.SystemQueries (contextRolTypes, contextType, rolPropertyTypes, mogelijkeBinding)
import Perspectives.TripleGetter (constructExternalPropertyGetter, constructInternalPropertyGetter, constructInverseRolGetter, constructRolGetter, constructRolLookup, constructRolPropertyGetter, constructRolPropertyLookup, constructTripleGetterFromEffectExpression)
import Perspectives.Utilities (onNothing, onNothing')
import Prelude (Unit, bind, const, discard, pure, unit, ($), (<$>), (<*>), (<<<), (<>), (==), (>=>), (>>=))

-- | From a qualified name for a Rol and its context, construct a function that computes the instances of that Rol for a given context.
-- | The Rol may be defined as computed.
rolQuery  :: forall e.
  RolName ->
  ContextID ->
  MonadPerspectivesQuery (AjaxAvarCache e) (TypedTripleGetter e)
rolQuery rn cid = do
  -- Is the type of rolType rn or one of its ancestors q:Query?
  (isAQuery :: Boolean) <- (toBoolean (contains "model:QueryAst$Query" (closure contextType)) rn)
  if isAQuery
    -- TODO: memorize!
    then do
      (TypedTripleGetter nameOfq q domain range) <- runMonadPerspectivesQueryCompiler cid (constructQueryFunction rn)
      pure $ TypedTripleGetter
        "saveInitialContext"
        (\cid -> do
          putQueryVariable "#context" [cid]
          t@(Triple {object : values}) <- q cid
          pure t)
        domain
        range
    else do
      domain <- lift $ getContextTypeF rn
      pure $ constructRolLookup (localName rn) domain rn

  where
    localName :: RolName -> LocalName
    localName qn = unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI qn)

-- | From a qualified name for a Property, construct a function that computes the values of that Property for a given rol.
-- | The Property may be defined as computed.
propertyQuery  :: forall e.
  PropertyName ->
  ContextID ->
  MonadPerspectivesQuery (AjaxAvarCache e) (TypedTripleGetter e)
propertyQuery pn cid = do
  -- Is the type of propertyType or one of its ancestors q:Query?
  (isAQuery :: Boolean) <- (toBoolean (contains "model:QueryAst$Query" (closure contextType)) pn)
  if isAQuery
    then do
      (TypedTripleGetter nameOfq q domain range) <- runMonadPerspectivesQueryCompiler pn (constructQueryFunction pn)
      pure $ TypedTripleGetter "saveInitialRol"
        (\rid -> do
          putQueryVariable "#rol" [rid]
          t@(Triple {object : values}) <- q rid
          pure t)
        domain
        range
    else do
      domain <- lift $ getContextTypeF pn
      pure $ constructRolPropertyLookup (localName pn) domain pn

  where
    localName :: RolName -> LocalName
    localName qn = unsafePartial $ fromJust (deconstructLocalNameFromDomeinURI qn)

-- | From the id of a context that is a description of a Query, construct a function that computes the value of that
-- | query from the id of an entity.
-- TODO: voeg state toe waarin bijgehouden wordt welke variabelen al gedefinieerd zijn, zodat je kunt stoppen als vooruit verwezen wordt. Houdt daar ook het domein van de querystap bij.
constructQueryFunction :: forall e.
  ContextID ->
  MonadPerspectivesQueryCompiler (AjaxAvarCache e) (TypedTripleGetter e)
constructQueryFunction typeDescriptionID = do
  queryStepType <- lift $ lift $ onNothing (errorMessage "no type found" "")
    (firstOnly getContextType typeDescriptionID)
  domain <- getQueryStepDomain
  case queryStepType of
    "model:QueryAst$constructExternalPropertyGetter" ->
      applyPropertyConstructor constructExternalPropertyGetter queryStepType
    "model:QueryAst$constructExternalPropertyLookup" -> do
      localName <- onNothing' (errorMessage "no propertyName" queryStepType) (deconstructLocalNameFromDomeinURI typeDescriptionID)
      qn <- lift $ lift $ onNothing (errorMessage ("no qualified name for " <> localName) queryStepType) (lookupQualifiedPropertyNameInRolTypeTelescope localName domain)
      pure $ constructExternalPropertyGetter qn domain qn
    "model:QueryAst$constructInternalPropertyGetter" ->
      applyPropertyConstructor constructInternalPropertyGetter queryStepType
    "model:QueryAst$constructInternalPropertyLookup" -> do
      localName <- onNothing' (errorMessage "no propertyName" queryStepType) (deconstructLocalNameFromDomeinURI typeDescriptionID)
      qn <- lift $ lift $ onNothing (errorMessage ("no qualified name for " <> localName) queryStepType) (lookupQualifiedPropertyNameInRolTypeTelescope localName domain)
      pure $ constructInternalPropertyGetter qn domain qn
    "model:QueryAst$propertyQuery" -> do
      propertyName <- lift $ lift $ makeFunction "model:QueryAst$propertyQuery$property" (getRol "model:QueryAst$propertyQuery$property") typeDescriptionID
      lift $ propertyQuery domain propertyName
    "model:QueryAst$constructRolPropertyGetter" ->
      applyPropertyConstructor constructRolPropertyGetter queryStepType
    "model:QueryAst$constructRolPropertyLookup" -> do
      localName <- onNothing' (errorMessage "no propertyName" queryStepType) (deconstructLocalNameFromDomeinURI typeDescriptionID)
      qn <- lift $ lift $ onNothing (errorMessage ("no qualified name for " <> localName) queryStepType) (lookupQualifiedPropertyNameInRolTypeTelescope localName domain)
      pure $ constructRolPropertyGetter qn domain qn
    "model:QueryAst$rolQuery" -> do
      rolName <- lift $ lift $ makeFunction "model:QueryAst$rolQuery$property" (getRol "model:QueryAst$rolQuery$property") typeDescriptionID
      lift $ rolQuery domain rolName
    -- Superfluous: can always be replaced by "model:QueryAst$rolQuery".
    "model:QueryAst$constructRolLookup" -> do
      localName <- onNothing' (errorMessage "no rolName" queryStepType) (deconstructLocalNameFromDomeinURI typeDescriptionID)
      qn <- lift $ lift $ onNothing (errorMessage ("no qualified name for " <> localName) queryStepType) (lookupQualifiedRolNameInContextTypeHierarchy localName domain)
      pure $ constructRolGetter qn domain
    -- Superfluous: can always be replaced by "model:QueryAst$constructRolLookup" and by "model:QueryAst$rolQuery".
    "model:QueryAst$constructRolGetter" -> do
      rolName <- lift $ lift $ onNothing (errorMessage "no rolName" queryStepType) (firstOnly (getRol "model:QueryAst$constructRolGetter$rol") typeDescriptionID)
      pure $ constructRolGetter rolName domain
    "model:QueryAst$constructInverseRolGetter" -> do
      rolName <- lift $ lift $ onNothing (errorMessage "no rolName" queryStepType) (firstOnly (getRol "model:QueryAst$constructRolGetter$rol") typeDescriptionID)
      pure $ constructInverseRolGetter rolName domain rolName
    "model:QueryAst$rolesOf" ->
      lift $ lift $ rolesOf <$> (onNothing (errorMessage "no context" queryStepType) (firstOnly (getRol "model:QueryAst$rolesOf$context") queryStepType))
    "model:QueryAst$notEmpty" -> applyUnaryCombinator notEmpty queryStepType
    "model:QueryAst$closure" -> applyUnaryCombinator closure queryStepType
    "model:QueryAst$closure'" -> applyUnaryCombinator closure' queryStepType
    "model:QueryAst$lastElement'" -> applyUnaryCombinator lastElement queryStepType
    "model:QueryAst$compose" -> applyBinaryCombinator compose queryStepType
    "model:QueryAst$concat" -> do
      (operandIds :: Array ID) <- lift $ lift $ getRolByLocalName "operand" typeDescriptionID
      (operands :: Array (TypedTripleGetter e)) <- traverse (withQueryCompilerEnvironment <<< constructQueryFunction) operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" queryStepType) (unsnoc operands)
      lift $ lift $ foldM concat last init
    "model:QueryAst$filter" -> do
      criteriumId <- lift $ lift $ onNothing (errorMessage "no criterium" queryStepType)
        (firstOnly (getRolByLocalName "criterium") typeDescriptionID)
      candidateId <- lift $ lift $ onNothing (errorMessage "no candidates" queryStepType)
        (firstOnly (getRolByLocalName "criterium") typeDescriptionID)
      filter <$> withQueryCompilerEnvironment (constructQueryFunction criteriumId) <*> withQueryCompilerEnvironment (constructQueryFunction candidateId)
    "model:QueryAst$Constant" -> do
      lift $ lift $ constant <$> onNothing (errorMessage "no constant value provided" queryStepType) (firstOnly (getExternalProperty "model:QueryAst$Constant$value") typeDescriptionID)
    "model:QueryAst$Variable" -> do
      variableName <- lift $ lift $ onNothing (errorMessage "no variable name found" queryStepType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      variableType <- onNothing (error $ "Variable " <> variableName <> " is either not declared or its declaration is invalid.") (getQueryVariableType variableName)
      pure $ constructTripleGetterFromEffectExpression
        ("model:QueryAst$Variable_" <> variableName)
        (const $ readQueryVariable variableName)
        domain
        variableType
    "model:QueryAst$setVariable" -> do
      variableName <- lift $ lift $ onNothing (errorMessage "no variable name found" queryStepType)
        (firstOnly (getInternalProperty "model:QueryAst$Variable$name") typeDescriptionID)
      valueDescriptionID <- lift $ lift $ onNothing (errorMessage "no value found" queryStepType)
        (firstOnly (getRolByLocalName "value") typeDescriptionID)
      valueQuery@(TypedTripleGetter nameOfq q d r) <- withQueryCompilerEnvironment $ constructQueryFunction valueDescriptionID
      predicateName <- pure ("set_" <> variableName <> "_" <> nameOfq)
      pure $ TypedTripleGetter predicateName
        (\id -> do
          (Triple tf@{object : values}) <- q id
          putQueryVariable variableName values
          pure $ Triple tf {predicate = predicateName})
        d
        r
    -- Any other argument will be passed as is, thus implementing that we can create arbitrary contexts.
    _ -> pure $ constant typeDescriptionID

  where
    applyPropertyConstructor :: (PropertyName -> Domain -> Range -> TypedTripleGetter e)
      -> ID
      -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (TypedTripleGetter e)
    applyPropertyConstructor f queryStepType = do
      (domain :: Domain) <- getQueryStepDomain
      propertyId <- lift $ lift $ onNothing (errorMessage "no property found" queryStepType)
        (firstOnly (getRolByLocalName "property") typeDescriptionID)
      -- Is this property type available on the type identified by domain?
      lift $ lift $ guardRolHasProperty domain propertyId
      putQueryStepDomain propertyId
      pure $ f propertyId domain propertyId

    applyUnaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e )
      -> ID
      -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (TypedTripleGetter e)
    applyUnaryCombinator c queryStepType = do
      (domain :: Domain) <- getQueryStepDomain
      queryId <- lift $ lift $ onNothing (errorMessage "no query found" queryStepType)
        (firstOnly (getRolByLocalName "query") typeDescriptionID)
      tg@(TypedTripleGetter _ _ dom range) <- (withQueryCompilerEnvironment (constructQueryFunction queryId) >>= pure <<< c)
      -- The domain dom of the triplegetter must be equal to or a specialisation of domain.
      -- However, no unary combinators change the domain of the tripleGetter. Hence, the actual
      -- check has been performed in the recursive call to constructQueryFunction.
      -- guardSubsumes domain dom
      putQueryStepDomain range
      pure tg

    applyBinaryCombinator :: (TypedTripleGetter e -> TypedTripleGetter e -> TypedTripleGetter e)
      -> ID
      -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (TypedTripleGetter e)
    applyBinaryCombinator c queryStepType = do
      (operandIds :: Array ID) <- lift $ lift $ getRolByLocalName "operand" typeDescriptionID
      operands <- traverse (withQueryCompilerEnvironment <<< constructQueryFunction) operandIds
      {init, last} <- onNothing' (errorMessage "too few operands" queryStepType) (unsnoc operands)
      pure $ foldl c last init

    errorMessage :: String -> String -> Error
    errorMessage s t = error ("constructEffectStatement: " <> s <> " for: " <> t <> " " <> typeDescriptionID)

    firstOnly :: ObjectsGetter e -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String))
    firstOnly g = g >=> (pure <<< head)

    guardRolHasProperty :: RolID -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Unit
    guardRolHasProperty rolId propertyId = do
      ln <- onNothing' (error $ "Property identifier is not well formed: " <> propertyId) (deconstructLocalNameFromDomeinURI propertyId)
      qn <- onNothing (error $ "Property " <> propertyId <> " is not defined for " <> rolId) (lookupQualifiedPropertyNameInRolTypeTelescope ln rolId)
      if qn == propertyId then pure unit else throwError (error $ "The property " <> propertyId <> " was specified, but " <> qn <> " was found!")

    -- | Throws an error if the second type is not a specialization of the first.
    guardSubsumes :: ContextID -> ContextID -> MonadPerspectives (AjaxAvarCache e) Unit
    guardSubsumes superType subType = pure unit

-- | Given a local propertyname and the typedescription of a rol, find the qualified version of that local name.
-- | It can be found on any level in the hierarchy of types from which the rol type derives.
-- TODO: verander dit naar Aspecten!
lookupQualifiedPropertyNameInRolTypeHierarchy :: forall e. LocalName -> ContextID -> MonadPerspectives (AjaxAvarCache e) (Maybe ID)
lookupQualifiedPropertyNameInRolTypeHierarchy ln rolTypeId = do
  (definedProperties :: Triple e) <- rolTypeId ## rolPropertyTypes
  qn <- pure $ rolTypeId <> "$" <> ln
  case elemIndex qn (tripleObjects definedProperties) of
    (Just p) -> pure $ Just qn
    Nothing -> do
      super <- getContextTypeF rolTypeId
      if super == rolTypeId
        then pure Nothing
        else lookupQualifiedPropertyNameInRolTypeHierarchy ln super

-- | Given a local rolname and the typedescription of a context, find the qualified version of that local name.
-- | It can be found on any level in the hierarchy of types from which the context type derives.
lookupQualifiedRolNameInContextTypeHierarchy :: forall e. LocalName -> ContextID -> MonadPerspectives (AjaxAvarCache e) (Maybe ID)
lookupQualifiedRolNameInContextTypeHierarchy ln contextTypeId = do
  (definedRollen :: Triple e) <- contextTypeId ## contextRolTypes
  qn <- pure $ contextTypeId <> "$" <> ln
  case elemIndex qn (tripleObjects definedRollen) of
    (Just p) -> pure $ Just qn
    Nothing -> do
      super <- getContextTypeF contextTypeId
      if super == contextTypeId
        then pure Nothing
        else lookupQualifiedRolNameInContextTypeHierarchy ln super
    -- (getContextTypeF contextTypeId) >>= (lookupQualifiedRolNameInContextTypeHierarchy ln)

-- | Given a local propertyname and the typedescription of a rol, find the qualified version of that local name in the type hierarchy of that typedescription. If not found, extend the search recursively to the mogelijkeBinding of the rol type.
lookupQualifiedPropertyNameInRolTypeTelescope :: forall e. LocalName -> ContextID -> MonadPerspectives (AjaxAvarCache e) (Maybe ID)
lookupQualifiedPropertyNameInRolTypeTelescope ln rolTypeId = do
  qn <- lookupQualifiedPropertyNameInRolTypeHierarchy ln rolTypeId
  case qn of
    (Just n) -> pure $ Just n
    Nothing -> (rolTypeId ## mogelijkeBinding) >>= (pure <<< head <<< tripleObjects) >>= maybe (pure Nothing)  (lookupQualifiedPropertyNameInRolTypeTelescope ln)
