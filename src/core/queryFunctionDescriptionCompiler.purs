module Perspectives.QueryFunctionDescriptionCompiler where

import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.StrMap (singleton)
import Perspectives.ContextAndRole (defaultContextRecord, defaultRolRecord)
import Perspectives.CoreTypes (MonadPerspectives, MonadPerspectivesQueryCompiler, getQueryStepDomain)
import Perspectives.Deltas (onNothing, onNothing')
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolID, RolName)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI, guardWellFormedNess, isInNamespace)
import Perspectives.PerspectEntiteit (cacheEntiteitPreservingVersion)
import Perspectives.Property (getRol)
import Perspectives.QueryAST (ElementaryQueryStep(..), QueryStep)
import Perspectives.QueryCombinators (toBoolean)
import Perspectives.QueryCompiler (lookupQualifiedPropertyNameInRolTypeTelescope, lookupQualifiedRolNameInContextTypeHierarchy)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), PropertyValueWithComments(..), binding, toRevision)
import Perspectives.SystemQueries (isContext)
import Prelude (Unit, bind, discard, flip, ifM, pure, unit, ($), (<<<), (<>), (==), (>>=), (>>>))

compileElementaryQueryStep :: forall e. ElementaryQueryStep -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
compileElementaryQueryStep s name = case s of
  Constant v -> createContextWithInternalProperty name (q "constant") v
  Variable v -> createContextWithInternalProperty name (q "variable") v
  RolesOf cid -> (createContextWithSingleRole name (q "rolesOf") cid)
  Binding -> createUndecoratedContext name (q "binding")
  Context -> createUndecoratedContext name (q "context")
  UseCache -> createUndecoratedContext name (q "useCache")
  IgnoreCache -> createUndecoratedContext name (q "ignoreCache")
  Identity -> createUndecoratedContext name (q "identity")
  Type ->
    ifM (getQueryStepDomain >>= (lift <<< toBoolean isContext))
      (createUndecoratedContext name (q "contextType"))
      (createUndecoratedContext name (q "rolType"))
  BuitenRol -> createUndecoratedContext name (q "buitenRol")
  IedereRolInContext -> createUndecoratedContext name (q "iedereRolInContext")
  RolTypen -> createUndecoratedContext name (q "rolTypen")
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
  q :: String -> String
  q ln = "model:QueryAst$" <> ln

  qualify :: String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) String
  qualify ln = onNothing ("There is no property " <> ln <> " defined for role " <> ln)
        (getQueryStepDomain >>= lift <<< lift <<< (lookupQualifiedPropertyNameInRolTypeTelescope ln))

  isInNamespace' :: ID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) Boolean
  isInNamespace' id = do
    ns <- getQueryStepDomain
    pure $ isInNamespace id ns

compileQueryStep :: forall e. QueryStep -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
compileQueryStep s = pure "string"

-- | Constructs a context with a single role that is bound to the role identified by the third parameter.
-- | The role (type) name is defined in the type identified by the second parameter.
-- Example:
-- q:constructRolPropertyGetter name
--   $property => pol:Aangifte$Aangever$betrouwbaarheid
createContextWithSingleRole :: forall e. String -> ContextID -> ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createContextWithSingleRole name typeId bindingValue = do
  ln <- guardWellFormedNess deconstructLocalNameFromDomeinURI name
  parameter <- lift $ onNothing ("No parameter found for " <> typeId) (lift ((getRol "model:Perspectives$rolInContext" typeId) >>= pure <<< head)) -- qualified name of parameter
  parameterLn <- guardWellFormedNess deconstructLocalNameFromDomeinURI parameter
  parameterInstanceName <- pure (name <> "$" <> parameterLn) -- qualified name of rol instance
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
            }
      , rolInContext = singleton parameter [parameterInstanceName]
      })
  lift $ lift $ cacheEntiteitPreservingVersion (name <> "_buitenRol")
    (PerspectRol defaultRolRecord
      { _id = name <> "_buitenRol"
      , pspType = "model:Perspectives$BuitenRol"
      , context = name
      , binding = binding $ typeId <> "_buitenRol"
      })
  lift $ lift $ cacheEntiteitPreservingVersion parameterInstanceName
    (PerspectRol defaultRolRecord
      { _id = parameterInstanceName
      , pspType = parameter
      , context = name
      , binding = binding bindingValue
      })
  pure $ name <> "_buitenRol"

createUndecoratedContext :: forall e. String -> ContextID -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createUndecoratedContext name typeId = do
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
            }
      })
  lift $ lift $ cacheEntiteitPreservingVersion (name <> "_buitenRol")
    (PerspectRol defaultRolRecord
      { _id = name <> "_buitenRol"
      , pspType = "model:Perspectives$BuitenRol"
      , context = name
      , binding = binding $ typeId <> "_buitenRol"
      })
  pure $ name <> "_buitenRol"

createContextWithInternalProperty :: forall e. String -> ContextID -> String -> MonadPerspectivesQueryCompiler (AjaxAvarCache e) ID
createContextWithInternalProperty name typeId propVal = do
  ln <- guardWellFormedNess deconstructLocalNameFromDomeinURI name
  parameter <- lift $ onNothing ("No parameter found for " <> typeId) (lift ((getRol "model:Perspectives$internalProperty" typeId) >>= pure <<< head)) -- qualified name of property
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
            , properties = singleton parameter
                (PropertyValueWithComments {commentBefore: [], commentAfter: [], value: [propVal]})
            }
      })
  lift $ lift $ cacheEntiteitPreservingVersion (name <> "_buitenRol")
    (PerspectRol defaultRolRecord
      { _id = name <> "_buitenRol"
      , pspType = "model:Perspectives$BuitenRol"
      , context = name
      , binding = binding $ typeId <> "_buitenRol"
      })
  pure $ name <> "_buitenRol"

guardRolHasProperty :: forall e. RolID -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Unit
guardRolHasProperty rolId propertyId = do
  ln <- onNothing' ("Property identifier is not well formed: " <> propertyId) (deconstructLocalNameFromDomeinURI propertyId)
  qn <- onNothing ("Property " <> propertyId <> " is not defined for " <> rolId) (lookupQualifiedPropertyNameInRolTypeTelescope ln rolId)
  if qn == propertyId then pure unit else throwError (error $ "The property " <> propertyId <> " was specified, but " <> qn <> " was found!")

guardContextHasRol :: forall e. ContextID -> RolName -> MonadPerspectives (AjaxAvarCache e) Unit
guardContextHasRol contextId rolId = do
  ln <- onNothing' ("Rol identifier is not well formed: " <> rolId) (deconstructLocalNameFromDomeinURI rolId)
  qn <- onNothing ("Rol " <> rolId <> " is not defined for " <> contextId) (lookupQualifiedRolNameInContextTypeHierarchy ln contextId)
  if qn == rolId then pure unit else throwError (error $ "The rol " <> rolId <> " was specified, but " <> qn <> " was found!")
