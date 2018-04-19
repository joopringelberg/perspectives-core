module Perspectives.QueryFunctionDescriptionCompiler where

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, head, singleton)
import Data.Either (Either(..), either, fromLeft, fromRight, isLeft)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import Perpectives.TypeChecker (checkContextForQualifiedRol, checkRolForQualifiedProperty, hasAspect)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQueryCompiler, getQueryStepDomain, getQueryVariableType, putQueryStepDomain, putQueryVariableType, tripleObjects, withQueryCompilerEnvironment, (##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolID, RolName)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI, guardWellFormedNess, isInNamespace)
import Perspectives.PerspectEntiteit (cacheEntiteitPreservingVersion)
import Perspectives.Property (getRol)
import Perspectives.QueryAST (ElementaryQueryStep(..), QueryStep(..))
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (lookupQualifiedPropertyNameInRolTypeTelescope)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..), binding, toRevision)
import Perspectives.SystemQueries (contextRolTypes, isContext)
import Perspectives.Utilities (ifNothing, onNothing)
import Prelude (class Monad, Unit, bind, discard, flip, ifM, pure, show, unit, ($), (*>), (<$>), (<*>), (<<<), (<>), (>>=), (>>>))

type Aspect = String
type Type = String

data UserMessage =
    MissingVariableDeclaration String
  | MissingAspect Type Aspect

type FD = Either UserMessage ID

-- This function creates a context that describes a query and is identified by contextId.
compileElementaryQueryStep :: forall e. ElementaryQueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
compileElementaryQueryStep s contextId = case s of
  Constant tp v -> putQueryStepDomain tp *> createContextWithInternalProperty contextId (q "constant") v
  Variable v -> ifNothing (getQueryVariableType v)
    (pure $ Left $ MissingVariableDeclaration v)
    \tp -> do
      putQueryStepDomain tp
      createContextWithInternalProperty contextId (q "variable") v
  RolesOf cid -> do
    dom <- getQueryStepDomain
    ifM (lift $ lift $ hasAspect "model:Perspectives$Context" dom)
      do
        tps <- lift $ lift (cid ## contextRolTypes)
        sumtype <- createSumType $ tripleObjects tps
        putQueryStepDomain sumtype
        createContextWithSingleRole contextId (q "iedereRolInContext") cid
      (pure $ Left $ MissingAspect dom "model:Perspectives$Context")
    -- isContext <- lift $ lift $ hasAspect "model:Perspectives$Context" dom
    -- if isContext
    --   then do
    --     tps <- lift $ lift (cid ## contextRolTypes)
    --     sumtype <- createSumType $ tripleObjects tps
    --     putQueryStepDomain sumtype
    --     createContextWithSingleRole contextId (q "iedereRolInContext") cid
    --   else pure $ Left $ MissingAspect dom "model:Perspectives$Context"
  Binding -> putQueryStepDomain (p "Rol") *> createUndecoratedContext contextId (q "binding")
  Context -> putQueryStepDomain (p "Context") *> createUndecoratedContext contextId (q "context")
  UseCache -> createUndecoratedContext contextId (q "useCache")
  IgnoreCache -> createUndecoratedContext contextId (q "ignoreCache")
  Identity -> createUndecoratedContext contextId (q "identity")
  -- TODO: moeten we dit niet vervangen door contextType en rolType?
  Type -> putQueryStepDomain (p "Context") *>
    ifM (getQueryStepDomain >>= (lift <<< toBoolean isContext))
      (createUndecoratedContext contextId (q "contextType")) -- TODO: dit is het enige alternatief dat ooit geevalueerd wordt!
      (createUndecoratedContext contextId (q "rolType"))
  BuitenRol -> putQueryStepDomain (p "Rol") *> createUndecoratedContext contextId (q "buitenRol")
  IedereRolInContext -> putQueryStepDomain (p "Rol") *> createUndecoratedContext contextId (q "iedereRolInContext")
  RolTypen -> putQueryStepDomain (p "Context") *> createUndecoratedContext contextId (q "rolTypen")
  Label -> createUndecoratedContext contextId (q "label")
  QualifiedProperty p -> do
    getQueryStepDomain >>= (lift <<< lift <<< (flip guardRolHasProperty p))
    ifM (isInNamespace' p)
      (createContextWithSingleRole contextId (q "constructRolPropertyGetter") p)
      (createContextWithSingleRole contextId (q "constructRolPropertyLookup") p)
  QualifiedInternalProperty p -> do
    getQueryStepDomain >>= (lift <<< lift <<< (flip guardRolHasProperty p))
    ifM (isInNamespace' p)
      (createContextWithSingleRole contextId (q "constructInternalPropertyGetter") p)
      (createContextWithSingleRole contextId (q "constructInternalPropertyLookup") p)
  QualifiedExternalProperty p -> do
    getQueryStepDomain >>= (lift <<< lift <<< (flip guardRolHasProperty p))
    ifM (isInNamespace' p)
      (createContextWithSingleRole contextId (q "constructExternalPropertyGetter") p)
      (createContextWithSingleRole contextId (q "constructExternalPropertyLookup") p)
  UnqualifiedProperty ln ->
    (qualify ln) >>= QualifiedProperty >>> (flip compileElementaryQueryStep contextId)
  UnqualifiedInternalProperty ln ->
    (qualify ln) >>= QualifiedInternalProperty >>> (flip compileElementaryQueryStep contextId)
  UnqualifiedExternalProperty ln ->
    (qualify ln) >>= QualifiedExternalProperty >>> (flip compileElementaryQueryStep contextId)
  QualifiedRol rn -> do
    getQueryStepDomain >>= (lift <<< lift <<< (flip guardContextHasRol rn))
    ifM (isInNamespace' rn)
      (createContextWithSingleRole contextId (q "constructRolGetter") rn)
      (createContextWithSingleRole contextId (q "constructRolLookup") rn)
  UnqualifiedRol ln ->
    (qualify ln) >>= QualifiedRol >>> (flip compileElementaryQueryStep contextId)

  where
  qualify :: String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) String
  qualify ln = onNothing (error $ "There is no property " <> ln <> " defined for role " <> ln)
        (getQueryStepDomain >>= lift <<< lift <<< (lookupQualifiedPropertyNameInRolTypeTelescope ln))

  isInNamespace' :: ID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) Boolean
  isInNamespace' id = do
    ns <- getQueryStepDomain
    pure $ isInNamespace id ns

-- This function creates a context that describes a query that is identified by contextId and that results from
-- applying a combinator to another query.
compileCombinatorQueryStep :: forall e. QueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
compileCombinatorQueryStep s contextId = case s of
  Filter cr ca -> compileCombinatorQueryStep cr (contextId <> "$criterium") `whenRight`
    \criterium -> compileCombinatorQueryStep ca (contextId <> "$candidates") `whenRight`
      \candidates -> createContext contextId (q "filter") [Tuple (q "filter$criterium") [criterium], Tuple (q "filter$candidates") [candidates]] []
  Concat oprnds -> do
    (operands :: Array (Either UserMessage (Tuple RolName (Array ID)))) <- traverseWithIndex (f "$concat") oprnds
    case find isLeft operands of
      Nothing -> createContext contextId (q "concat") (unsafePartial (fromRight <$> operands)) []
      (Just a) -> pure $ unsafePartial $ Left $ fromLeft a
  Compose oprnds -> do
    operands <- traverseWithIndex (f "$compose") oprnds
    case find isLeft operands of
      Nothing -> createContext contextId (q "compose") (unsafePartial (fromRight <$> operands)) []
      (Just a) -> pure $ unsafePartial $ Left $ fromLeft a
  NotEmpty qs -> compileUnaryStep qs "notEmpty"
  Closure qs -> compileUnaryStep qs "closure"
  Closure' qs -> compileUnaryStep qs "closure'"
  LastElement qs -> compileUnaryStep qs "laatste"
  Contains vqs qs -> compileCombinatorQueryStep vqs (contextId <> "$valueOrId") `whenRight`
    \value -> compileCombinatorQueryStep qs (contextId <> "$query") `whenRight`
      \query -> createContext contextId (q "contains") [Tuple (q "contains$valueOrId") [value], Tuple (q "contains$query") [query]] []
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
    \t -> (throwError (error $ "Variable '" <> var <> "' cannot be given a value, as it already has a value of type " <> t))

  Terminal es -> compileElementaryQueryStep es contextId

  where
    -- Compiles the description of the query represented by qs and then creates the description of a query
    -- that results from applying the combinator to it.
    compileUnaryStep :: QueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
    compileUnaryStep qs combinatorLocalName = compileCombinatorQueryStep qs (contextId <> "$" <> combinatorLocalName) >>=
      either
        (pure <<< Left)
        (createContextWithSingleRole contextId (q combinatorLocalName))

    f :: String
      -> Int
      -> QueryStep
      -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (Either UserMessage (Tuple RolName (Array ID)))
    f localName i qs = do
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

p :: String -> String
p ln = "model:Perspectives$" <> ln

createUndecoratedContext :: forall e. String -> ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createUndecoratedContext name typeId = createContext name typeId [] []

-- | Constructs a context with a single role that is bound to the role identified by 'bindingValue'
-- | Uses the type description provided by parameter 'contextType'.
-- | The role (type) name is retrieved as the value of the rolInContext role of that contextType.
-- Example of a corresponding CRL expression:
-- q:constructRolPropertyGetter name
--   $property => pol:Aangifte$Aangever$betrouwbaarheid
createContextWithSingleRole :: forall e. String -> ContextID -> ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createContextWithSingleRole contextId contextType bindingValue = do
  rolType <- lift $ onNothing (error $ "No rolType found for " <> contextType) (lift ((getRol "model:Perspectives$rolInContext" contextType) >>= pure <<< head)) -- qualified name of rolType
  rolInstanceId <- createRol rolType contextId bindingValue 0
  createContext contextId contextType [Tuple rolType [rolInstanceId]] []

createContextWithMultipleRoleInstances :: forall e. String -> ContextID -> RolID -> Array ContextID ->  MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createContextWithMultipleRoleInstances contextId contextType rolName bindings = do
  roles <- traverseWithIndex (\i b -> createRol rolName contextId b i) bindings
  createContext contextId contextType (Tuple rolName <<< singleton <$> roles) []

createSumType :: forall e. Array ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createSumType types = do
  sumtype <- (createContextWithMultipleRoleInstances "" "model:Perspectives$Sum" "model:Perspectives$Sum$alternative" types)
  pure (unsafePartial (fromRight sumtype))

-- | Constructs a context with a single property that is bound to the value identified by 'propVal'.
-- | Uses the type description provided by parameter 'contextType'.
-- | The property (type) name is retrieved as the value of the internalProperty role of that contextType.
createContextWithInternalProperty :: forall e. String -> ContextID -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
createContextWithInternalProperty contextId contextType propVal = do
  propertyName <- lift $ onNothing (error $ "No parameter found for " <> contextType) (lift ((getRol "model:Perspectives$internalProperty" contextType) >>= pure <<< head)) -- qualified name of property
  createContext contextId contextType [] [Tuple propertyName [propVal]]

-- rolName gives the type of the Rol to create.
-- contextId gives the identifier of the context that the Rol belongs to.
-- bindingValue is the identifier of a Rol that will be the value of the field binding.
createRol :: forall e. RolName -> ContextID -> ID -> Int -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createRol rolName contextId bindingValue i = do
  rolLn <- guardWellFormedNess deconstructLocalNameFromDomeinURI rolName
  rolInstanceName <- pure (contextId <> "$" <> rolLn <> (show i)) -- qualified name of rol instance
  lift $ lift $ cacheEntiteitPreservingVersion rolInstanceName
    (PerspectRol defaultRolRecord
      { _id = rolInstanceName
      , pspType = rolName
      , context = contextId
      , binding = binding bindingValue
      })
  pure rolInstanceName

createContext :: forall e.
  String ->
  ContextID ->
  Array (Tuple RolName (Array ID)) ->
  Array (Tuple PropertyName (Array String)) ->
  MonadPerspectivesQueryCompiler (AjaxAvarCache e) FD
-- TODO: voeg interne properties toe.
createContext name typeId roles properties = do
  ln <- guardWellFormedNess deconstructLocalNameFromDomeinURI name
  lift $ lift $ cacheEntiteitPreservingVersion name
   (PerspectContext defaultContextRecord
      { _id = name
      , _rev = toRevision Nothing
      , displayName = ln
      , pspType = typeId
      , binnenRol =
          PerspectRol defaultRolRecord
            { _id =  name <> "_binnenRol"
            , pspType = "model:Perspectives$BinnenRol"
            , binding = binding (name <> "_buitenRol")
            , properties = fromFoldable (createProperty <$> properties)
            }
      , rolInContext = fromFoldable roles
      })
  lift $ lift $ cacheEntiteitPreservingVersion (name <> "_buitenRol")
    (PerspectRol defaultRolRecord
      { _id = name <> "_buitenRol"
      , pspType = "model:Perspectives$BuitenRol"
      , context = name
      , binding = binding $ typeId <> "_buitenRol"
      })
  pure $ Right $ name <> "_buitenRol"

  where
  createProperty :: Tuple PropertyName (Array String) -> (Tuple PropertyName PropertyValueWithComments)
  createProperty (Tuple pn values) = Tuple pn (PropertyValueWithComments {commentBefore: [], commentAfter: [], value: values})

-- | This function tests whether the Rol type (identified by rolId) has the Aspect of having the property (identified by propertyId).
guardRolHasProperty :: forall e. RolID -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Unit
guardRolHasProperty rolId propertyId = do
  hasProperty <- checkRolForQualifiedProperty propertyId rolId
  if hasProperty then pure unit else throwError (error $ "The property " <> propertyId <> " cannot be found!")

guardContextHasRol :: forall e. ContextID -> RolName -> MonadPerspectives (AjaxAvarCache e) Unit
guardContextHasRol contextId rolId = do
  hasRol <- checkContextForQualifiedRol rolId contextId
  if hasRol then pure unit else throwError (error $ "The rol " <> rolId <> " cannot be found!")
