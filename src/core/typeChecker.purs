module Perpectives.TypeChecker where

import Control.Alt ((<|>))
import Data.Array (foldM, head, length, union)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (FD, MonadPerspectives, TypeID, TypedTripleGetter, UserMessage(..), ObjectsGetter, tripleObjects, tripleObjects_, (%%>>))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolName)
import Perspectives.Identifiers (deconstructNamespace, guardWellFormedNess)
import Perspectives.ModelBasedTripleGetters (aspectenDefMClosure, ownRollenDefM, propertiesDefM)
import Perspectives.ObjectGetterConstructors (getRol, unlessNull)
import Perspectives.ObjectsGetterComposition ((/-/), (\-\))
import Perspectives.QueryCombinators (contains, containsMatching, toBoolean, filter)
import Perspectives.RunMonadPerspectivesQuery ((##),runTypedTripleGetter, runMonadPerspectivesQuery)
import Perspectives.DataTypeObjectGetters (contextType, binding, context, rolType)
import Prelude (bind, flip, ifM, join, pure, ($), (&&), (<$>), (<*>), (<<<), (<>), (==), (>>=), (||))

-- TODO. DIT WERKT NIET VOOR INTERNE EN EXTERNE CONTEXT PROPERTIES.
-- erft een context interne- of externe properties van aspecten?
-- checkContextForQualifiedInternalProperty
-- checkContextForQualifiedExternalProperty
-- checkContextForUnQualifiedInternalProperty
-- checkContextForUnQualifiedExternalProperty

-- | `psp:Rol -> psp:Property -> Boolean`
checkRolForQualifiedProperty :: forall e. PropertyName -> RolName -> MonadPerspectives (AjaxAvarCache e) Boolean
checkRolForQualifiedProperty pn rn = do
  namespaceOfProperty <- guardWellFormedNess deconstructNamespace pn
  (&&) <$> checkRolHasAspect rn namespaceOfProperty <*> checkRolHasProperty namespaceOfProperty pn
  where
    checkRolHasAspect :: RolName -> RolName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkRolHasAspect rn' an = (||) <$> rn' `isOrHasAspect` an <*> checkMogelijkeBindingHasAspect rn' an

    checkMogelijkeBindingHasAspect :: RolName -> RolName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkMogelijkeBindingHasAspect rn' an = do
      b <- mogelijkeBinding rn'
      case head b of
        Nothing -> pure false
        (Just bd) -> do
          object <- alternatives bd
          case head object of
            Nothing -> checkRolForQualifiedProperty pn bd -- Not a Sum type
            otherwise -> foldM (\r alt -> checkRolForQualifiedProperty pn alt >>= pure <<< (&&) r) true object

    checkRolHasProperty :: RolName -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkRolHasProperty rn' pn' = runMonadPerspectivesQuery rn' (toBoolean (contains pn' propertiesDefM))

mogelijkeBinding :: forall e. ObjectsGetter e
mogelijkeBinding = (getRol "model:Perspectives$Rol$mogelijkeBinding") /-/ binding /-/ context

alternatives :: forall e. ObjectsGetter e
alternatives = (getRol "model:Perspectives$Sum$alternative") /-/ binding /-/ context

checkContextForQualifiedRol :: forall e. RolName -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
checkContextForQualifiedRol rn cn = do
  aspect <- guardWellFormedNess deconstructNamespace rn
  (&&) <$> cn `isOrHasAspect` aspect <*> checkContextHasRol aspect rn
  where
    checkContextHasRol :: RolName -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkContextHasRol cn' rn' = runMonadPerspectivesQuery cn' (toBoolean (contains rn' ownRollenDefM))

-- | Returns the Aspect that defines the property, or a usermessage indicating that property with the given
-- | local name can be found, or that several have been found.
checkRolForUnQualifiedProperty :: forall e. PropertyName -> RolName -> MonadPerspectives (AjaxAvarCache e) FD
checkRolForUnQualifiedProperty ln rn' = do
  aspects <- aspectsWithUnqualifiedProperty ln rn'
  case length aspects of
    0 -> pure $ Left $ MissingUnqualifiedProperty ln rn'
    1 -> pure $ Right $ unsafePartial $ fromJust $ head aspects
    otherwise -> pure $ Left $ MultipleDefinitions ln aspects
  where

    aspectsWithUnqualifiedProperty :: PropertyName -> ObjectsGetter e
    aspectsWithUnqualifiedProperty ln' rn = union <$> importedAspectsWithUnqualifiedProperty rn <*> aspectsFromMogelijkeBindingWithUnqualifiedProperty rn
      where
        importedAspectsWithUnqualifiedProperty :: ObjectsGetter e
        importedAspectsWithUnqualifiedProperty rn'' = (rn'' ## filter (hasUnqualifiedProperty ln') aspectenDefMClosure) >>= tripleObjects_

        aspectsFromMogelijkeBindingWithUnqualifiedProperty :: ObjectsGetter e
        aspectsFromMogelijkeBindingWithUnqualifiedProperty rn'' =
          (unlessNull
            ((mogelijkeBinding /-/ alternatives) \-\ aspectsWithUnqualifiedProperty ln')) rn''
          <|>
          (mogelijkeBinding /-/ aspectsWithUnqualifiedProperty ln') rn''

    hasUnqualifiedProperty :: PropertyName -> TypedTripleGetter e
    hasUnqualifiedProperty ln' = containsMatching
      (\rolName propertyName -> (rolName <> "$" <> ln') == propertyName)
      ("UnqualifiedProperty" <> ln')
      propertiesDefM

checkContextForUnQualifiedRol :: forall e. RolName -> ContextID -> MonadPerspectives (AjaxAvarCache e) FD
checkContextForUnQualifiedRol ln cn = do
  aspects <- aspectsWithUnqualifiedRol
  case length aspects of
    0 -> pure $ Left $ MissingUnqualifiedRol ln cn
    1 -> pure $ Right $ unsafePartial $ fromJust $ head aspects
    otherwise -> pure $ Left $ MultipleDefinitions ln aspects
  where

    aspectsWithUnqualifiedRol :: MonadPerspectives (AjaxAvarCache e) (Array ID)
    aspectsWithUnqualifiedRol = (cn ## filter (hasUnqualifiedRol ln) aspectenDefMClosure) >>= tripleObjects_

    hasUnqualifiedRol :: PropertyName -> TypedTripleGetter e
    hasUnqualifiedRol ln' = containsMatching
      (\contextName rolName -> (contextName <> "$" <> ln') == rolName)
      ("UnqualifiedRol" <> ln')
      ownRollenDefM

-- | True when both parameters are equal and also when the first has the second as aspect.
-- | If the aspect is a sum type, tries each of the alternatives.
-- | subtype `isOrHasAspect` aspect
isOrHasAspect :: forall e. ContextID -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
isOrHasAspect subtype aspect = do
  object <- alternatives aspect
  case head object of
    Nothing -> if aspect == subtype
      then pure true
      else subtype `importsAspect` aspect
    otherwise -> foldM (\r alt -> isOrHasAspect subtype alt >>= pure <<< (||) r) false object

-- | True iff one of the Aspecten of tp contains the given Aspect.
-- | tp `importsAspect` aspect
-- | Every type has the Aspect psp:ElkType.
importsAspect :: forall e. ContextID -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
importsAspect tp aspect = if aspect == "model:Perspectives$ElkType"
  then pure true
  else (flip runMonadPerspectivesQuery) (toBoolean (contains aspect aspectenDefMClosure)) tp

-- | True iff the type of the context equals the given type, or if its type has the given type as aspect.
-- | `psp:ContextInstance -> psp:Context -> Boolean`
contextHasType :: forall e. TypeID -> TypeID -> MonadPerspectives (AjaxAvarCache e) Boolean
contextHasType ctxt tp = do
  typeOfBinding <- ctxt %%>> contextType
  typeOfBinding `isOrHasAspect` tp

-- | True iff the type of the role equals the given type, or if its type has the given type as aspect.
-- | `psp:RolInstance -> psp:Rol -> Boolean`
rolHasType :: forall e. TypeID -> TypeID -> MonadPerspectives (AjaxAvarCache e) Boolean
rolHasType rol tp = do
  typeOfBinding <- rol %%>> rolType
  typeOfBinding `isOrHasAspect` tp

mostSpecificCommonAspect :: forall e. Array TypeID -> MonadPerspectives (AjaxAvarCache e) TypeID
mostSpecificCommonAspect types = do
  x <- traverse (runTypedTripleGetter aspectenDefMClosure) types
  aspects <- pure $ join (tripleObjects <$> x)
  foldM (\msca t -> ifM (t `isOrHasAspect` msca) (pure msca) (pure t)) "model:Perspectives$ElkType" aspects
