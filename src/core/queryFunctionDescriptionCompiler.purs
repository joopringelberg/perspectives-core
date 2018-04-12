module Perspectives.QueryFunctionDescriptionCompiler where

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
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
import Prelude (Unit, bind, discard, flip, ifM, otherwise, pure, show, unit, ($), (*>), (<$>), (<*>), (<<<), (<>), (==), (>>=), (>>>))

compileElementaryQueryStep :: forall e. ElementaryQueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
compileElementaryQueryStep s name = case s of
  Constant v -> createContextWithInternalProperty name (q "constant") v
  Variable v -> do
    tp <- onNothing (error $ "Variable " <> v <> " has not been declared!") (getQueryVariableType v)
    putQueryStepDomain tp
    createContextWithInternalProperty name (q "variable") v
  RolesOf cid -> putQueryStepDomain (p "Rol") *> (createContextWithSingleRole name (q "rolesOf") cid)
  Binding -> putQueryStepDomain (p "Rol") *> createUndecoratedContext name (q "binding")
  Context -> putQueryStepDomain (p "Context") *> createUndecoratedContext name (q "context")
  UseCache -> createUndecoratedContext name (q "useCache")
  IgnoreCache -> createUndecoratedContext name (q "ignoreCache")
  Identity -> createUndecoratedContext name (q "identity")
  Type -> putQueryStepDomain (p "Context") *>
    ifM (getQueryStepDomain >>= (lift <<< toBoolean isContext))
      (createUndecoratedContext name (q "contextType"))
      (createUndecoratedContext name (q "rolType"))
  BuitenRol -> putQueryStepDomain (p "Rol") *> createUndecoratedContext name (q "buitenRol")
  IedereRolInContext -> putQueryStepDomain (p "Rol") *> createUndecoratedContext name (q "iedereRolInContext")
  RolTypen -> putQueryStepDomain (p "Context") *> createUndecoratedContext name (q "rolTypen")
  Label -> createUndecoratedContext name (q "label")
  QualifiedProperty p -> do
    getQueryStepDomain >>= (lift <<< lift <<< (flip guardRolHasProperty p))
    ifM (isInNamespace' p)
      (createContextWithSingleRole name (q "constructRolPropertyGetter") p)
      (createContextWithSingleRole name (q "constructRolPropertyLookup") p)
  QualifiedInternalProperty p -> do
    getQueryStepDomain >>= (lift <<< lift <<< (flip guardRolHasProperty p))
    ifM (isInNamespace' p)
      (createContextWithSingleRole name (q "constructInternalPropertyGetter") p)
      (createContextWithSingleRole name (q "constructInternalPropertyLookup") p)
  QualifiedExternalProperty p -> do
    getQueryStepDomain >>= (lift <<< lift <<< (flip guardRolHasProperty p))
    ifM (isInNamespace' p)
      (createContextWithSingleRole name (q "constructExternalPropertyGetter") p)
      (createContextWithSingleRole name (q "constructExternalPropertyLookup") p)
  UnqualifiedProperty ln ->
    (qualify ln) >>= QualifiedProperty >>> (flip compileElementaryQueryStep name)
  UnqualifiedInternalProperty ln ->
    (qualify ln) >>= QualifiedInternalProperty >>> (flip compileElementaryQueryStep name)
  UnqualifiedExternalProperty ln ->
    (qualify ln) >>= QualifiedExternalProperty >>> (flip compileElementaryQueryStep name)
  QualifiedRol rn -> do
    getQueryStepDomain >>= (lift <<< lift <<< (flip guardContextHasRol rn))
    ifM (isInNamespace' rn)
      (createContextWithSingleRole name (q "constructRolGetter") rn)
      (createContextWithSingleRole name (q "constructRolLookup") rn)
  UnqualifiedRol ln ->
    (qualify ln) >>= QualifiedRol >>> (flip compileElementaryQueryStep name)

  where
  qualify :: String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) String
  qualify ln = onNothing (error $ "There is no property " <> ln <> " defined for role " <> ln)
        (getQueryStepDomain >>= lift <<< lift <<< (lookupQualifiedPropertyNameInRolTypeTelescope ln))

  isInNamespace' :: ID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) Boolean
  isInNamespace' id = do
    ns <- getQueryStepDomain
    pure $ isInNamespace id ns

compileQueryStep :: forall e. QueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
compileQueryStep s name = case s of
  Filter cr ca -> do
    criterium <- compileQueryStep cr (name <> "$criterium")
    candidates <- compileQueryStep ca (name <> "$candidates")
    createContext name (q "filter") [Tuple (q "filter$criterium") [criterium], Tuple (q "filter$candidates") [candidates]] []
  Concat oprnds -> do
    operands <- traverseWithIndex (f "$concat") oprnds
    createContext name (q "concat") operands []
  Compose oprnds -> do
    operands <- traverseWithIndex (f "$compose") oprnds
    createContext name (q "compose") operands []
  NotEmpty qs -> compileUnaryStep qs "notEmpty"
  Closure qs -> compileUnaryStep qs "closure"
  Closure' qs -> compileUnaryStep qs "closure'"
  LastElement qs -> compileUnaryStep qs "lastElement"
  Contains vqs qs -> do
    value <- compileQueryStep vqs (name <> "$valueOrId")
    query <- compileQueryStep qs (name <> "$query")
    createContext name (q "contains") [Tuple (q "contains$valueOrId") [value], Tuple (q "contains$query") [query]] []
  SetVariable var qs -> ifNothing (getQueryVariableType var)
    do
      (Tuple v tp) <- withQueryCompilerEnvironment
        (Tuple <$> compileQueryStep qs (name <> "$value") <*> getQueryStepDomain)
      putQueryVariableType name tp
      createContext name (q "setVariable") [Tuple (q "setVariable$value") [v]]
        [Tuple (q "setVariable$name") [var]]
    \t -> (throwError (error $ "Variable '" <> var <> "' cannot be given a value, as it already has a value of type " <> t))

  Terminal es -> compileElementaryQueryStep es name

  where
    compileUnaryStep :: QueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
    compileUnaryStep qs localTypeName = compileQueryStep qs (name <> "$" <> localTypeName) >>= createContextWithSingleRole name (q localTypeName)

    f :: String -> Int -> QueryStep -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) (Tuple RolName (Array ID))
    f localName i qs = do
      nm <- pure (name <> localName <> (show i))
      result <- compileQueryStep qs nm
      pure $ Tuple nm [result]

q :: String -> String
q ln = "model:QueryAst$" <> ln

p :: String -> String
p ln = "model:Perspectives$" <> ln

createUndecoratedContext :: forall e. String -> ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createUndecoratedContext name typeId = createContext name typeId [] []

-- | Constructs a context with a single role that is bound to the role identified by the third parameter.
-- | The role (type) name is defined in the type identified by the second parameter.
-- Example:
-- q:constructRolPropertyGetter name
--   $property => pol:Aangifte$Aangever$betrouwbaarheid
createContextWithSingleRole :: forall e. String -> ContextID -> ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createContextWithSingleRole name typeId bindingValue = do
  parameter <- lift $ onNothing (error $ "No parameter found for " <> typeId) (lift ((getRol "model:Perspectives$rolInContext" typeId) >>= pure <<< head)) -- qualified name of parameter
  rolInstanceId <- createRol parameter name bindingValue
  createContext name typeId [Tuple parameter [rolInstanceId]] []

createContextWithInternalProperty :: forall e. String -> ContextID -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createContextWithInternalProperty name typeId propVal = do
  parameter <- lift $ onNothing (error $ "No parameter found for " <> typeId) (lift ((getRol "model:Perspectives$internalProperty" typeId) >>= pure <<< head)) -- qualified name of property
  createContext name typeId [] [Tuple parameter [propVal]]

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

guardRolHasProperty :: forall e. RolID -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Unit
guardRolHasProperty rolId propertyId = do
  ln <- onNothing' (error $ "Property identifier is not well formed: " <> propertyId) (deconstructLocalNameFromDomeinURI propertyId)
  qn <- onNothing (error $ "Property " <> propertyId <> " is not defined for " <> rolId) (lookupQualifiedPropertyNameInRolTypeTelescope ln rolId)
  if qn == propertyId then pure unit else throwError (error $ "The property " <> propertyId <> " was specified, but " <> qn <> " was found!")

guardContextHasRol :: forall e. ContextID -> RolName -> MonadPerspectives (AjaxAvarCache e) Unit
guardContextHasRol contextId rolId = do
  ln <- onNothing' (error $ "Rol identifier is not well formed: " <> rolId) (deconstructLocalNameFromDomeinURI rolId)
  qn <- onNothing (error $ "Rol " <> rolId <> " is not defined for " <> contextId) (lookupQualifiedRolNameInContextTypeHierarchy ln contextId)
  if qn == rolId then pure unit else throwError (error $ "The rol " <> rolId <> " was specified, but " <> qn <> " was found!")
