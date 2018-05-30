module Perspectives.QueryFunctionDescriptionCompiler where

import Control.Monad.Eff.Exception (error)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, head, singleton)
import Data.Either (Either(..), either, fromLeft, fromRight, isLeft)
import Data.Maybe (Maybe(..), fromJust)
import Data.StrMap (fromFoldable)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartial)
import Perpectives.TypeChecker (checkContextForQualifiedRol, checkContextForUnQualifiedRol, checkRolForQualifiedProperty, checkRolForUnQualifiedProperty, contextHasType, isOrHasAspect, mostSpecificCommonAspect)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (FD, MonadPerspectivesQueryCompiler, TypeID, UserMessage(..), getQueryStepDomain, getQueryVariableType, putQueryStepDomain, putQueryVariableType, tripleObjects, withQueryCompilerEnvironment)
import Perspectives.DataTypeTripleGetters (contextTypeM)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolID, RolName)
import Perspectives.Identifiers (binnenRol, buitenRol, deconstructLocalNameFromDomeinURI, guardWellFormedNess, isInNamespace)
import Perspectives.ModelBasedTripleGetters (bindingDefM, ownRollenDefM)
import Perspectives.ObjectGetterConstructors (getRol)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectEntiteit (cacheEntiteitPreservingVersion)
import Perspectives.QueryAST (ElementaryQueryStep(..), QueryStep(..))
import Perspectives.RunMonadPerspectivesQuery (runTypedTripleGetter, (##), (##>), (##>>))
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..), binding, toRevision)
import Perspectives.DataTypeObjectGetters (binding, context) as DTG
import Perspectives.Utilities (ifNothing, onNothing)
import Prelude (class Monad, bind, discard, ifM, pure, show, ($), (*>), (<$>), (<*>), (<<<), (<>), (>>=))

-- This function creates a context that describes a query. This context will be identified by contextId.
-- | The domain of the resulting function is the value of 'domain' in the
-- | QueryCompilerEnvironment. It should be provided by the application of
-- | runMonadPerspectivesQueryCompiler.
compileElementaryQueryStep :: forall e. ElementaryQueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
compileElementaryQueryStep s contextId = case s of
  Constant tp v -> putQueryStepDomain tp *> createContextWithExternalProperty contextId (q "constant") v
  Variable v -> ifNothing (getQueryVariableType v)
    (pure $ Left $ MissingVariableDeclaration v)
    \tp -> do
      putQueryStepDomain tp
      createContextWithExternalProperty contextId (q "variable") v
  RolesOf cid -> ensureAspect (psp "Context")
    do
      putQueryStepDomain (psp "Rol")
      createContextWithSingleRole contextId (q "rolesOf") cid
  Binding -> ensureAspect (psp "Rol")
    do
      dom <- getQueryStepDomain
      ifNothing (lift (dom ##> bindingDefM))
        (pure $ Left $ MissingMogelijkeBinding dom)
        \bindingType -> do
          putQueryStepDomain bindingType
          createDataTypeGetterDescription contextId "binding"
  Context -> ensureAspect (psp "Rol")
    do
      rolType <- getQueryStepDomain
      putQueryStepDomain $ unsafePartial $ fromJust $ deconstructLocalNameFromDomeinURI rolType
      createDataTypeGetterDescription contextId "context"
  Identity -> createDataTypeGetterDescription contextId  "identityM"
  Type -> do
    dom <- getQueryStepDomain
    ifM (lift $ dom `isOrHasAspect` (psp "Context"))
      do
        tp <- lift (dom ##>> contextTypeM) >>= putQueryStepDomain
        createDataTypeGetterDescription contextId "contextTypeM"
      do
        tp <- lift (dom ##>> contextTypeM) >>= putQueryStepDomain
        createDataTypeGetterDescription contextId "rolType"
  BuitenRol -> ensureAspect (psp "Context")
    (putQueryStepDomain (psp "Rol") *> createDataTypeGetterDescription contextId "buitenRol")
  IedereRolInContext -> ensureAspect (psp "Context")
    do
      dom <- getQueryStepDomain
      tps <- lift (dom ## ownRollenDefM)
      sumtype <- createSumType $ tripleObjects tps
      putQueryStepDomain sumtype
      createDataTypeGetterDescription contextId "iedereRolInContext"
  RolTypen -> ensureAspect (psp "Context")
    do
      getQueryStepDomain >>= lift <<< runTypedTripleGetter ownRollenDefM >>= pure <<< tripleObjects >>= lift <<< mostSpecificCommonAspect >>= putQueryStepDomain
      createDataTypeGetterDescription contextId "typeVanIedereRolInContext"
  Label -> ensureAspect (psp "Context")
    (putQueryStepDomain (psp "String") *> createDataTypeGetterDescription contextId "label")
  -- TODO. Als de property bijgedragen wordt door een Aspect en niet afgebeeld is op een bindingProperty,
  -- is hij óók lokaal gerepresenteerd en moet dan dus door constructRolPropertyGetter gevonden worden.
  QualifiedProperty p -> do
    dom <- getQueryStepDomain
    -- TODO. Pas de constructie hieronder toe op de andere gevallen.
    ensureRolHasProperty dom p
      ((ifM (lift $ p `contextHasType` "model:Perspectives$Function")
          (createContextWithSingleRole contextId (q "propertyQuery") p)
          (ifM (isInQueryStepDomain p)
            (createPropertyGetterDescription contextId "constructRolPropertyGetter" p)
            (createPropertyGetterDescription contextId "constructRolPropertyLookup" p)))
        `thenPutQueryStepDomain` p)
  QualifiedInternalProperty p -> do
    dom <- getQueryStepDomain
    ensureRolHasProperty dom p
      (ifM (lift $ p `contextHasType` "model:Perspectives$Function")
          (createContextWithSingleRole contextId (q "propertyQuery") p)
          (ifM (isInQueryStepDomain p)
            (createPropertyGetterDescription contextId "constructInternalPropertyGetter" p)
            (createPropertyGetterDescription contextId "constructInternalPropertyLookup" p)
            ))
          `thenPutQueryStepDomain` p
  QualifiedExternalProperty p -> do
    dom <- getQueryStepDomain
    -- TODO. Hier moet een controle komen op externe properties van een context.
    ensureRolHasProperty dom p
      (ifM (lift $ p `contextHasType` "model:Perspectives$Function")
          (createContextWithSingleRole contextId (q "propertyQuery") p)
          (ifM (isInQueryStepDomain p)
            (createPropertyGetterDescription contextId "constructExternalPropertyGetter" p)
            (createPropertyGetterDescription contextId "constructExternalPropertyLookup" p)
            ))
        `thenPutQueryStepDomain` p
  UnqualifiedProperty ln -> ensureAspect (psp "Rol")
    do
      rolType <- getQueryStepDomain
      aspectOrMessage <- lift $ checkRolForUnQualifiedProperty ln rolType
      case aspectOrMessage of
        (Left um) -> pure $ Left um
        (Right aspect) -> do
          let qn = aspect <> "$" <> ln
          (ifM (lift $ qn `contextHasType` "model:Perspectives$Function")
            (createPropertyGetterDescription contextId "propertyQuery" qn)
            (ifM (isInQueryStepDomain aspect)
              (createPropertyGetterDescription contextId "constructRolPropertyGetter" qn)
              (createPropertyGetterDescription contextId "constructRolPropertyLookup" qn)))
            `thenPutQueryStepDomain` qn
  UnqualifiedInternalProperty ln -> ensureAspect (psp "Rol")
    do
      rolType <- getQueryStepDomain
      aspectOrMessage <- lift $ checkRolForUnQualifiedProperty ln rolType
      case aspectOrMessage of
        (Left um) -> pure $ Left um
        (Right aspect) -> do
          let qn = aspect <> "$" <> ln
          (ifM (lift $ qn `contextHasType` "model:Perspectives$Function")
            (createPropertyGetterDescription contextId "propertyQuery" qn)
            (ifM (isInQueryStepDomain qn)
              (createPropertyGetterDescription contextId "constructInternalPropertyGetter" qn)
              (createPropertyGetterDescription contextId "constructInternalPropertyLookup" qn)))
            `thenPutQueryStepDomain` qn
  UnqualifiedExternalProperty ln -> ensureAspect (psp "Rol")
    do
      rolType <- getQueryStepDomain
      aspectOrMessage <- lift $ checkRolForUnQualifiedProperty ln rolType
      case aspectOrMessage of
        (Left um) -> pure $ Left um
        (Right aspect) -> do
          let qn = aspect <> "$" <> ln
          (ifM (lift $ qn `contextHasType` "model:Perspectives$Function")
            (createPropertyGetterDescription contextId "propertyQuery" qn)
            (ifM (isInQueryStepDomain qn)
              (createPropertyGetterDescription contextId "constructExternalPropertyGetter" qn)
              (createPropertyGetterDescription contextId "constructExternalPropertyLookup" qn)))
            `thenPutQueryStepDomain` qn
  QualifiedRol rn -> do
    dom <- getQueryStepDomain
    ensureContextHasRol dom rn
      (ifM (lift $ rn `contextHasType` "model:Perspectives$Function")
        (createRolGetterDescription contextId "rolQuery" rn)
        (ifM (isInQueryStepDomain rn)
          (createRolGetterDescription contextId "constructRolGetter" rn)
          (createRolGetterDescription contextId "constructRolLookup" rn)))
        `thenPutQueryStepDomain` rn
  UnqualifiedRol ln -> ensureAspect (psp "Context")
    do
      contextType <- getQueryStepDomain
      aspectOrMessage <- lift $ checkContextForUnQualifiedRol ln contextType
      case aspectOrMessage of
        (Left um) -> pure $ Left um
        (Right aspect) -> do
          let qn = aspect <> "$" <> ln
          -- TODO: controleer ook hier op berekende Rollen.
          (ifM (isInQueryStepDomain qn)
            (createRolGetterDescription contextId "constructRolGetter" qn)
            (createRolGetterDescription contextId "constructRolLookup" qn))
            `thenPutQueryStepDomain` qn
  where

  thenPutQueryStepDomain :: MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD -> ID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
  thenPutQueryStepDomain m dom = do
    r <- m
    putQueryStepDomain dom
    pure r

  isInQueryStepDomain :: ID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) Boolean
  isInQueryStepDomain id = do
    ns <- getQueryStepDomain
    pure $ id `isInNamespace` ns

  ensureAspect ::
    TypeID
    -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
    -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
  ensureAspect aspect mv = do
    dom <- getQueryStepDomain
    ifM (lift $ dom `isOrHasAspect` aspect)
      mv
      (pure $ Left $ MissingAspect dom aspect)

-- | `psp:Rol -> psp:Property -> FD`
  ensureRolHasProperty ::
    RolID
    -> PropertyName
    -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
    -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
  ensureRolHasProperty rolId propertyId mv =
    ifM (lift $ checkRolForQualifiedProperty propertyId rolId)
      mv
      (pure $ Left $ MissingQualifiedProperty propertyId rolId)

  ensureContextHasRol ::
    ContextID
    -> RolName
    -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
    -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
  ensureContextHasRol contextId' rolId mv =
    ifM (lift $ checkContextForQualifiedRol rolId contextId')
      mv
      (pure $ Left $ MissingQualifiedRol rolId contextId')

-- This function creates a context that describes a query that is identified by
-- contextId and that results from applying a combinator to another query.
compileCombinatorQueryStep :: forall e. QueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
compileCombinatorQueryStep s contextId = case s of
  Filter cr ca -> compileCombinatorQueryStep cr (contextId <> "$criterium") `whenRight`
    \criterium -> compileCombinatorQueryStep ca (contextId <> "$candidates") `whenRight`
      \candidates -> createContext contextId (q "filter") [Tuple (q "filter$criterium") [criterium], Tuple (q "filter$candidates") [candidates]] []
  Concat oprnds -> do
    (operands :: Array (Either UserMessage (Tuple RolName (Array ID)))) <- traverseWithIndex (compileOperand "$concat") oprnds
    case find isLeft operands of
      Nothing -> do
        (operands' :: Array (Tuple RolName (Array ID))) <- pure (unsafePartial (fromRight <$> operands))
        operandIds <- pure $ unsafePartial $ fromJust <<< head <<< snd <$> operands'
        (lift $ mostSpecificCommonAspect operandIds) >>= putQueryStepDomain
        createContext contextId (q "concat") operands' []
      (Just a) -> pure $ unsafePartial $ Left $ fromLeft a
  Compose oprnds -> do
    operands <- traverseWithIndex (compileOperand "$compose") oprnds
    case find isLeft operands of
      Nothing -> createContext contextId (q "compose") (unsafePartial (fromRight <$> operands)) []
      (Just a) -> pure $ unsafePartial $ Left $ fromLeft a
  NotEmpty qs -> putQueryStepDomain (psp "Boolean") *> compileUnaryStep qs "notEmpty"
  Closure qs -> compileUnaryStep qs "closure"
  Closure' qs -> compileUnaryStep qs "closure'"
  UseCache qs -> compileUnaryStep qs "UseCache"
  IgnoreCache qs -> compileUnaryStep qs "IgnoreCache'"
  LastElement qs -> compileUnaryStep qs "laatste"
  Contains vqs qs -> compileCombinatorQueryStep vqs (contextId <> "$valueOrId") `whenRight`
    \value -> compileCombinatorQueryStep qs (contextId <> "$query") `whenRight`
      \query -> putQueryStepDomain (psp "Boolean") *> createContext contextId (q "contains") [Tuple (q "contains$valueOrId") [value], Tuple (q "contains$query") [query]] []
  SetVariable var qs -> ifNothing (getQueryVariableType var)
    do
      (Tuple v' tp) <- withQueryCompilerEnvironment
        (Tuple <$> compileCombinatorQueryStep qs (contextId <> "$value") <*> getQueryStepDomain)
      case v' of
        (Left um) -> pure $ Left um
        (Right v) -> do
          putQueryVariableType contextId tp
          createContext contextId (q "setVariable") [Tuple (q "setVariable$value") [v]]
            [Tuple (q "setVariable$name") [var]]
    \t -> pure $ Left $ VariableAlreadyDeclaredAs var t

  Terminal es -> compileElementaryQueryStep es contextId

  where
    -- Compiles the description of the query represented by qs and then creates the description of a query
    -- that results from applying the combinator to it.
    compileUnaryStep :: QueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
    compileUnaryStep qs combinatorLocalName = compileCombinatorQueryStep qs (contextId <> "$" <> combinatorLocalName) >>=
      either
        (pure <<< Left)
        (createContextWithSingleRole contextId (q combinatorLocalName))

    compileOperand :: String
      -> Int
      -> QueryStep
      -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (Either UserMessage (Tuple RolName (Array ID)))
    compileOperand localName i qs = do
        nm <- pure (contextId <> localName <> (show i))
        (result :: Either UserMessage ID) <- compileCombinatorQueryStep qs nm
        case result of
          (Left um) -> pure $ Left um
          (Right id) -> pure $ Right (Tuple nm [id])

    whenRight :: forall a b c m. Monad m =>
      m (Either a b) ->
      (b -> m (Either a c)) ->
      m (Either a c)
    whenRight m f = do
      eab <- m
      case eab of
        (Left a) -> pure $ Left a
        (Right b) -> f b

q :: String -> String
q ln = "model:QueryAst$" <> ln

psp :: String -> String
psp ln = "model:Perspectives$" <> ln

-- | Constructs a context with a single role that is bound to the role identified by 'bindingValue'
-- | Uses the type description provided by parameter 'contextType'.
-- | The role (type) name is retrieved as the value of the rolInContext role of that contextType.
-- Example of a corresponding CRL expression:
-- q:constructRolPropertyGetter name
--   $property => pol:Aangifte$Aangever$betrouwbaarheid
createContextWithSingleRole :: forall e. String -> ContextID -> ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createContextWithSingleRole contextId contextType bindingValue = do
  rolType <- onNothing
    (error $ "No rolType found for " <> contextType)
    (lift $
      ((getRol "model:Perspectives$Context$rolInContext") /-/ DTG.binding /-/ DTG.context) contextType >>= pure <<< head) -- qualified name of rolType
  rolInstanceId <- createRol rolType contextId (buitenRol bindingValue) 0
  createContext contextId contextType [Tuple rolType [rolInstanceId]] []

createContextWithMultipleRoleInstances :: forall e. String -> ContextID -> RolID -> Array ContextID ->  MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createContextWithMultipleRoleInstances contextId contextType rolName bindings = do
  roles <- traverseWithIndex (\i b -> createRol rolName contextId b i) (buitenRol <$> bindings)
  createContext contextId contextType (Tuple rolName <<< singleton <$> roles) []

createSumType :: forall e. Array ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createSumType types = do
  sumtype <- (createContextWithMultipleRoleInstances "" "model:Perspectives$Sum" "model:Perspectives$Sum$alternative" types)
  pure (unsafePartial (fromRight sumtype))

-- | Constructs a context with a single property that is bound to the value identified by 'propVal'.
-- | Uses the type description provided by parameter 'contextType'.
-- | The property (type) name is retrieved as the value of the internalProperty role of that contextType.
createContextWithExternalProperty :: forall e. String -> ContextID -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createContextWithExternalProperty contextId contextType propVal = do
  propertyName <- onNothing (error $ "No parameter found for " <> contextType) (lift ((getRol "model:Perspectives$Context$buitenenRolBeschrijving$rolProperty" contextType) >>= pure <<< head)) -- qualified name of property
  createContext contextId contextType [] [Tuple propertyName [propVal]]

createDataTypeGetterDescription :: forall e. ContextID -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createDataTypeGetterDescription contextId functionName =
  createContext contextId (q "DataTypeGetter") [] [Tuple (q "DataTypeGetter$buitenRolBeschrijvingfunctionName") [functionName]]

createPropertyGetterDescription :: forall e. ContextID -> String -> PropertyName -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createPropertyGetterDescription contextId functionName propertyName = do
  rolInstanceId <- createRol (q "PropertyGetter$property") contextId (buitenRol propertyName) 0
  createContext contextId (q "PropertyGetter") [Tuple (q "PropertyGetter$property") [rolInstanceId]] [Tuple (q "PropertyGetter$buitenRolBeschrijving$functionName") [functionName]]

createRolGetterDescription :: forall e. ContextID -> String -> RolName -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createRolGetterDescription contextId functionName rolName = do
  rolInstanceId <- createRol (q "RolGetter$rol") contextId (buitenRol rolName) 0
  createContext contextId (q "RolGetter") [Tuple (q "RolGetter$rol") [rolInstanceId]] [Tuple (q "RolGetter$buitenRolBeschrijving$functionName") [functionName]]

-- rolName gives the type of the Rol to create.
-- contextId gives the identifier of the context that the Rol belongs to.
-- bindingValue is the identifier of a Rol that will be the value of the field binding.
createRol :: forall e. RolName -> ContextID -> ID -> Int -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createRol rolName contextId bindingValue i = do
  rolLn <- guardWellFormedNess deconstructLocalNameFromDomeinURI rolName
  rolInstanceName <- pure (contextId <> "$" <> rolLn <> (show i)) -- qualified name of rol instance
  lift $ cacheEntiteitPreservingVersion rolInstanceName
    (PerspectRol defaultRolRecord
      { _id = rolInstanceName
      , pspType = rolName
      , context = contextId
      , binding = binding bindingValue
      })
  pure rolInstanceName

-- | Create a context with the given Roles and the properties as EXTERNAL properties.
createContext :: forall e.
  String ->
  ContextID ->
  Array (Tuple RolName (Array ID)) ->
  Array (Tuple PropertyName (Array String)) ->
  MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createContext name typeId roles properties = do
  ln <- guardWellFormedNess deconstructLocalNameFromDomeinURI name
  lift $ cacheEntiteitPreservingVersion name
   (PerspectContext defaultContextRecord
      { _id = name
      , _rev = toRevision Nothing
      , displayName = ln
      , pspType = typeId
      , buitenRol = (buitenRol name)
      , binnenRol =
          PerspectRol defaultRolRecord
            { _id =  binnenRol name
            , pspType = typeId <> "$binnenRolBeschrijving"
            , binding = binding (buitenRol name)
            }
      , rolInContext = fromFoldable roles
      })
  lift $ cacheEntiteitPreservingVersion (buitenRol name)
    (PerspectRol defaultRolRecord
      { _id = buitenRol name
      , pspType = typeId <> "$buitenRolBeschrijving"
      , context = name
      , binding = binding $ buitenRol typeId
      , properties = fromFoldable (createProperty <$> properties)
      })
  pure $ Right $ name

  where
  createProperty :: Tuple PropertyName (Array String) -> (Tuple PropertyName PropertyValueWithComments)
  createProperty (Tuple pn values) = Tuple pn (PropertyValueWithComments {commentBefore: [], commentAfter: [], value: values})
