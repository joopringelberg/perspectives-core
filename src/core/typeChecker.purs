module Perspectives.TypeChecker where

import Data.Array (foldM, head, length)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (FD, MonadPerspectives, UserMessage(..), (%%>>))
import Perspectives.DataTypeObjectGetters (contextType, rolBindingDef, rolType)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (deconstructNamespace, guardWellFormedNess, LocalName)
import Perspectives.ObjectGetterConstructors (closureOfAspect, closureOfAspectRol, contains, getUnqualifiedContextRol, searchUnqualifiedPropertyDefinition, searchUnqualifiedRolDefinition, toBoolean, alternatives, mogelijkeBinding)
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.PerspectivesTypes (class RolClass, AnyContext, ContextDef(..), PropertyDef, RolDef(..), AnyDefinition, typeWithPerspectivesTypes)
import Prelude (bind, ifM, join, pure, ($), (&&), (<$>), (<*>), (<<<), (==), (>>=), (||), (=<<), map)

-- TODO. DIT WERKT NIET VOOR INTERNE EN EXTERNE CONTEXT PROPERTIES.
-- erft een context interne- of externe properties van aspecten? Ja, dat kan.
-- checkContextForQualifiedInternalProperty
-- checkContextForQualifiedExternalProperty
-- checkContextForUnQualifiedInternalProperty
-- checkContextForUnQualifiedExternalProperty

-- | Returns true iff all of the following holds:
-- |  * the PropertyDef name is well-formed;
-- |  * the RolDef has the namespace of the PropertyDef as AspectRol (or it is that namespace itself). This consists
-- |    of two conditions, one of which must be true:
-- |      * the RolDef (recursively) has the AspectRol;
-- |      * the mogelijkeBinding of the RolDef (recursively) has the AspectRol.
-- |  * the RolDef actually has the fully qualified property as part of its definition.
checkRolForQualifiedProperty :: forall e. PropertyDef -> RolDef -> MonadPerspectives (AjaxAvarCache e) Boolean
checkRolForQualifiedProperty pn rn = do
  (namespaceOfProperty :: RolDef) <- pure <<< RolDef =<< (guardWellFormedNess deconstructNamespace $ unwrap pn)
  (&&) <$> checkRolHasAspect rn namespaceOfProperty <*> checkRolHasProperty namespaceOfProperty pn
  where
    checkRolHasAspect :: RolDef -> RolDef -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkRolHasAspect rn' an = (||) <$> rn' `isOrHasAspectRol` an <*> checkMogelijkeBindingHasAspect rn' an

    checkMogelijkeBindingHasAspect :: RolDef -> RolDef -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkMogelijkeBindingHasAspect rn' an = do
      b <- typeWithPerspectivesTypes mogelijkeBinding rn'
      case head b of
        Nothing -> pure false
        (Just bd) -> do
          (object :: Array AnyContext) <- alternatives bd
          case head object of
            Nothing -> checkRolForQualifiedProperty pn (RolDef bd) -- Not a Sum type
            otherwise -> foldM (\r alt -> checkRolForQualifiedProperty pn alt >>= pure <<< (&&) r) true (map RolDef object)

    -- | Does the RolDef or one of its prototypes have a $rolProperty bound to (the BuitenRol of) the propertyDef?
    checkRolHasProperty :: RolDef -> PropertyDef -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkRolHasProperty rn' pn' = (toBoolean (contains (unwrap pn') (getUnqualifiedContextRol "rolProperty" /-/ rolBindingDef)) (unwrap rn'))

-- | True if the first RolDef recursively has the second RolDef as AspectRol, or if they are the same.
isOrHasAspectRol :: forall e. RolDef -> RolDef -> MonadPerspectives (AjaxAvarCache e) Boolean
isOrHasAspectRol subtype aspectRol =
  if aspectRol == subtype
    then pure true
    else if aspectRol == RolDef "model:Perspectives$ElkType"
      then pure true
      else (toBoolean (contains aspectRol closureOfAspectRol)) subtype

-- | Returns the Aspect that defines the property, or a usermessage indicating that property with the given
-- | local name can be found, or that several have been found.
-- TODO: ObjectGetterConstrutors.searchUnqualifiedPropertyDefinition vervult dezelfde functie (maar geeft de PropertyDef terug - althans, als daar is ingebouwd dat hij ook op de mogelijkeBinding graaf zoekt).
-- Deze functie wordt alleen gebruikt in de QueryFunctionDescriptionCompiler.
checkRolForUnQualifiedProperty :: forall e. LocalName -> RolDef -> MonadPerspectives (AjaxAvarCache e) FD
checkRolForUnQualifiedProperty ln rn = do
  (propdefs :: Array PropertyDef) <- searchUnqualifiedPropertyDefinition ln rn
  case length propdefs of
    0 -> pure $ Left $ MissingUnqualifiedProperty ln (unwrap rn)
    1 -> pure $ Right $ unwrap $ unsafePartial $ fromJust $ head propdefs
    otherwise -> pure $ Left $ MultipleDefinitions ln (map unwrap propdefs)

-- | Looks for the Aspect(s) that define an unqualified Rol with the given local name.
checkContextForUnQualifiedRol :: forall e. LocalName -> ContextDef -> MonadPerspectives (AjaxAvarCache e) FD
checkContextForUnQualifiedRol ln cn = do
  (roldefs :: Array RolDef) <- searchUnqualifiedRolDefinition ln cn
  case length roldefs of
    0 -> pure $ Left $ MissingUnqualifiedRol ln (unwrap cn)
    1 -> pure $ Right $ unwrap $ unsafePartial $ fromJust $ head roldefs
    otherwise -> pure $ Left $ MultipleDefinitions ln (map unwrap roldefs)

-- | True when both parameters are equal and also when the first has the second as aspect.
-- | If the aspect is a sum type, tries each of the alternatives.
-- | subtype `isOrHasAspect` aspect
isOrHasAspect :: forall e. ContextDef -> ContextDef -> MonadPerspectives (AjaxAvarCache e) Boolean
isOrHasAspect subtype aspect =
  if subtype == aspect
    then pure true
    else if aspect == ContextDef "model:Perspectives$ElkType"
      then pure true
      else toBoolean (contains (unwrap aspect) closureOfAspect) (unwrap subtype)

-- | True iff the type of the context equals the given type, or if its type has the given type as aspect.
-- | context `contextHasType` type
contextHasType :: forall e. AnyContext -> ContextDef -> MonadPerspectives (AjaxAvarCache e) Boolean
contextHasType ctxt tp = do
  (typeOfBinding :: AnyDefinition) <- ctxt %%>> contextType
  ContextDef typeOfBinding `isOrHasAspect` tp

-- | True iff the type of the role equals the given type, or if its type has the given type as aspect.
-- | `psp:RolInstance -> psp:Rol -> Boolean`
rolHasType :: forall r e. RolClass r => r -> RolDef -> MonadPerspectives (AjaxAvarCache e) Boolean
rolHasType rol tp = do
  (typeOfBinding :: RolDef) <- rol %%>> rolType
  (ContextDef $ unwrap typeOfBinding) `isOrHasAspect` (ContextDef $ unwrap tp)

mostSpecificCommonAspect :: forall e. Array AnyDefinition -> MonadPerspectives (AjaxAvarCache e) ContextDef
mostSpecificCommonAspect types = do
  x <- traverse closureOfAspect types
  (aspects :: Array AnyContext) <- pure $ join x
  foldM (\msca t -> ifM (t `isOrHasAspect` msca) (pure msca) (pure t))
    (ContextDef "model:Perspectives$ElkType")
    (map ContextDef aspects)
