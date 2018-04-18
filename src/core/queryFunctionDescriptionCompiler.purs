module Perspectives.QueryFunctionDescriptionCompiler where

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Perpectives.TypeChecker (checkContextForQualifiedRol, checkRolForQualifiedProperty)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQueryCompiler, getQueryStepDomain, getQueryVariableType, putQueryStepDomain, putQueryVariableType, withQueryCompilerEnvironment)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolID, RolName)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI, guardWellFormedNess, isInNamespace)
import Perspectives.PerspectEntiteit (cacheEntiteitPreservingVersion)
import Perspectives.Property (getRol)
import Perspectives.QueryAST (ElementaryQueryStep(..), QueryStep(..))
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (lookupQualifiedPropertyNameInRolTypeTelescope, lookupQualifiedRolNameInContextTypeHierarchy)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..), binding, toRevision)
import Perspectives.SystemQueries (isContext)
import Perspectives.Utilities (ifNothing, onNothing, onNothing')
import Prelude (Unit, bind, discard, flip, ifM, pure, show, unit, ($), (*>), (<$>), (<*>), (<<<), (<>), (==), (>>=), (>>>))

-- This function creates a context that describes a query and is identified by contextId.
compileElementaryQueryStep :: forall e. ElementaryQueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
compileElementaryQueryStep s contextId = case s of
  Constant v -> createContextWithInternalProperty contextId (q "constant") v
  Variable v -> do
    tp <- onNothing (error $ "Variable " <> v <> " has not been declared!") (getQueryVariableType v)
    putQueryStepDomain tp
    createContextWithInternalProperty contextId (q "variable") v
  RolesOf cid -> putQueryStepDomain (p "Rol") *> (createContextWithSingleRole contextId (q "iedereRolInContext") cid)
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
compileCombinatorQueryStep :: forall e. QueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
compileCombinatorQueryStep s contextId = case s of
  Filter cr ca -> do
    criterium <- compileCombinatorQueryStep cr (contextId <> "$criterium")
    candidates <- compileCombinatorQueryStep ca (contextId <> "$candidates")
    createContext contextId (q "filter") [Tuple (q "filter$criterium") [criterium], Tuple (q "filter$candidates") [candidates]] []
  Concat oprnds -> do
    operands <- traverseWithIndex (f "$concat") oprnds
    createContext contextId (q "concat") operands []
  Compose oprnds -> do
    operands <- traverseWithIndex (f "$compose") oprnds
    createContext contextId (q "compose") operands []
  NotEmpty qs -> compileUnaryStep qs "notEmpty"
  Closure qs -> compileUnaryStep qs "closure"
  Closure' qs -> compileUnaryStep qs "closure'"
  LastElement qs -> compileUnaryStep qs "laatste"
  Contains vqs qs -> do
    value <- compileCombinatorQueryStep vqs (contextId <> "$valueOrId")
    query <- compileCombinatorQueryStep qs (contextId <> "$query")
    createContext contextId (q "contains") [Tuple (q "contains$valueOrId") [value], Tuple (q "contains$query") [query]] []
  SetVariable var qs -> ifNothing (getQueryVariableType var)
    do
      (Tuple v tp) <- withQueryCompilerEnvironment
        (Tuple <$> compileCombinatorQueryStep qs (contextId <> "$value") <*> getQueryStepDomain)
      putQueryVariableType contextId tp
      createContext contextId (q "setVariable") [Tuple (q "setVariable$value") [v]]
        [Tuple (q "setVariable$name") [var]]
    \t -> (throwError (error $ "Variable '" <> var <> "' cannot be given a value, as it already has a value of type " <> t))

  Terminal es -> compileElementaryQueryStep es contextId

  where
    -- Compiles the description of the query represented by qs and then creates the description of a query
    -- that results from applying the combinator to it.
    compileUnaryStep :: QueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
    compileUnaryStep qs combinatorLocalName = compileCombinatorQueryStep qs (contextId <> "$" <> combinatorLocalName) >>= createContextWithSingleRole contextId (q combinatorLocalName)

    f :: String -> Int -> QueryStep -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (Tuple RolName (Array ID))
    f localName i qs = do
      nm <- pure (contextId <> localName <> (show i))
      result <- compileCombinatorQueryStep qs nm
      pure $ Tuple nm [result]

q :: String -> String
q ln = "model:QueryAst$" <> ln

p :: String -> String
p ln = "model:Perspectives$" <> ln

createUndecoratedContext :: forall e. String -> ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createUndecoratedContext name typeId = createContext name typeId [] []

-- | Constructs a context with a single role that is bound to the role identified by 'bindingValue'
-- | Uses the type description provided by parameter 'contextType'.
-- | The role (type) name is retrieved as the value of the rolInContext role of that contextType.
-- Example of a corresponding CRL expression:
-- q:constructRolPropertyGetter name
--   $property => pol:Aangifte$Aangever$betrouwbaarheid
createContextWithSingleRole :: forall e. String -> ContextID -> ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createContextWithSingleRole contextId contextType bindingValue = do
  rolType <- lift $ onNothing (error $ "No rolType found for " <> contextType) (lift ((getRol "model:Perspectives$rolInContext" contextType) >>= pure <<< head)) -- qualified name of rolType
  rolInstanceId <- createRol rolType contextId bindingValue
  createContext contextId contextType [Tuple rolType [rolInstanceId]] []

-- | Constructs a context with a single property that is bound to the value identified by 'propVal'.
-- | Uses the type description provided by parameter 'contextType'.
-- | The property (type) name is retrieved as the value of the internalProperty role of that contextType.
createContextWithInternalProperty :: forall e. String -> ContextID -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createContextWithInternalProperty contextId contextType propVal = do
  propertyName <- lift $ onNothing (error $ "No parameter found for " <> contextType) (lift ((getRol "model:Perspectives$internalProperty" contextType) >>= pure <<< head)) -- qualified name of property
  createContext contextId contextType [] [Tuple propertyName [propVal]]

-- rolName gives the type of the Rol to create.
-- contextId gives the identifier of the context that the Rol belongs to.
-- bindingValue is the identifier of a Rol that will be the value of the field binding.
createRol :: forall e. RolName -> ContextID -> ID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createRol rolName contextId bindingValue = do
  rolLn <- guardWellFormedNess deconstructLocalNameFromDomeinURI rolName
  rolInstanceName <- pure (contextId <> "$" <> rolLn) -- qualified name of rol instance
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
  MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
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
  pure $ name <> "_buitenRol"

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
