module Perpectives.TypeChecker where

import Data.Array (foldM, head, intersect, length, union)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (FD, MonadPerspectives, Triple(..), TypeID, TypedTripleGetter, UserMessage(..), tripleGetter2function, tripleObjects, tripleObjects_)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolName)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI, guardWellFormedNess)
import Perspectives.Property (getContextTypeF)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (contains, containsMatching, toBoolean, filter)
import Perspectives.RunMonadPerspectivesQuery ((##), runTypedTripleGetter, runMonadPerspectivesQuery)
import Perspectives.SystemQueries (aspecten, binding, contextOwnRolTypes, mogelijkeBinding, rolPropertyTypes)
import Perspectives.TripleGetter (constructRolGetter)
import Prelude (bind, flip, ifM, join, pure, ($), (&&), (<$>), (<*>), (<<<), (<>), (==), (>>=), (||))

checkRolForQualifiedProperty :: forall e. PropertyName -> RolName -> MonadPerspectives (AjaxAvarCache e) Boolean
checkRolForQualifiedProperty pn rn = do
  aspect <- guardWellFormedNess deconstructLocalNameFromDomeinURI pn
  (&&) <$> checkRolHasAspect rn aspect <*> checkRolHasProperty aspect pn
  where
    checkRolHasAspect :: RolName -> RolName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkRolHasAspect rn an = (||) <$> checkTypeImportsAspect rn an <*> checkMogelijkeBindingHasAspect rn an

    checkMogelijkeBindingHasAspect :: RolName -> RolName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkMogelijkeBindingHasAspect rn an = do
      b <- runMonadPerspectivesQuery rn (tripleGetter2function mogelijkeBinding)
      case b of
        Nothing -> pure false
        (Just bd) -> do
          (Triple{object} ) <- bd ## alternatives
          case head object of
            Nothing -> checkRolForQualifiedProperty pn bd -- Not a Sum type
            otherwise -> foldM (\r alt -> checkRolForQualifiedProperty pn alt >>= pure <<< (&&) r) true object

    checkRolHasProperty :: RolName -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkRolHasProperty rn pn = runMonadPerspectivesQuery rn (toBoolean (contains pn rolPropertyTypes))

checkTypeImportsAspect :: forall e. ID -> ID -> MonadPerspectives (AjaxAvarCache e) Boolean
checkTypeImportsAspect typeId an = runMonadPerspectivesQuery typeId (toBoolean (contains an aspecten))

alternatives :: forall e. TypedTripleGetter e
alternatives = (constructRolGetter "model:Perspectives$Sum$alternative") >-> binding

checkContextForQualifiedRol :: forall e. RolName -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
checkContextForQualifiedRol rn cn = do
  aspect <- guardWellFormedNess deconstructLocalNameFromDomeinURI rn
  (&&) <$> checkTypeImportsAspect cn aspect <*> checkContextHasRol aspect rn
  where
    checkContextHasRol :: RolName -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkContextHasRol cn rn = runMonadPerspectivesQuery cn (toBoolean (contains rn contextOwnRolTypes))

-- | Returns the Aspect that defines the property, or a usermessage indicating that property with the given
-- | local name can be found, or that several have been found.
checkRolForUnQualifiedProperty :: forall e. PropertyName -> RolName -> MonadPerspectives (AjaxAvarCache e) FD
checkRolForUnQualifiedProperty ln rn = do
  aspects <- aspectsWithUnqualifiedProperty ln rn
  case length aspects of
    0 -> pure $ Left $ MissingUnqualifiedProperty ln rn
    1 -> pure $ Right $ unsafePartial $ fromJust $ head aspects
    otherwise -> pure $ Left $ MultipleDefinitions ln aspects
  where

    aspectsWithUnqualifiedProperty :: PropertyName -> RolName -> MonadPerspectives (AjaxAvarCache e) (Array ID)
    aspectsWithUnqualifiedProperty ln rn = union <$> importedAspectsWithUnqualifiedProperty <*> aspectsFromMogelijkeBindingWithUnqualifiedProperty
      where
        importedAspectsWithUnqualifiedProperty :: MonadPerspectives (AjaxAvarCache e) (Array ID)
        importedAspectsWithUnqualifiedProperty = (rn ## filter (hasUnqualifiedProperty ln) aspecten) >>= tripleObjects_

        aspectsFromMogelijkeBindingWithUnqualifiedProperty :: MonadPerspectives (AjaxAvarCache e) (Array ID)
        aspectsFromMogelijkeBindingWithUnqualifiedProperty = do
          b <- runMonadPerspectivesQuery rn (tripleGetter2function mogelijkeBinding)
          case b of
            Nothing -> pure []
            (Just bd) -> do
              (Triple{object} ) <- bd ## alternatives
              case head object of
                Nothing -> aspectsWithUnqualifiedProperty ln bd -- Not a Sum type
                otherwise -> foldM (\r alt -> aspectsWithUnqualifiedProperty ln alt >>= pure <<< intersect r) [] object


    hasUnqualifiedProperty :: PropertyName -> TypedTripleGetter e
    hasUnqualifiedProperty ln = containsMatching
      (\rolName propertyName -> (rolName <> "$" <> ln) == propertyName)
      ("UnqualifiedProperty" <> ln)
      rolPropertyTypes

checkContextForUnQualifiedRol :: forall e. RolName -> ContextID -> MonadPerspectives (AjaxAvarCache e) FD
checkContextForUnQualifiedRol ln cn = do
  aspects <- aspectsWithUnqualifiedRol
  case length aspects of
    0 -> pure $ Left $ MissingUnqualifiedRol ln cn
    1 -> pure $ Right $ unsafePartial $ fromJust $ head aspects
    otherwise -> pure $ Left $ MultipleDefinitions ln aspects
  where

    aspectsWithUnqualifiedRol :: MonadPerspectives (AjaxAvarCache e) (Array ID)
    aspectsWithUnqualifiedRol = (cn ## filter (hasUnqualifiedRol ln) aspecten) >>= tripleObjects_

    hasUnqualifiedRol :: PropertyName -> TypedTripleGetter e
    hasUnqualifiedRol ln = containsMatching
      (\contextName rolName -> (contextName <> "$" <> ln) == rolName)
      ("UnqualifiedRol" <> ln)
      contextOwnRolTypes

hasAspect :: forall e. ContextID -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
hasAspect aspect subtype = if aspect == subtype
  then pure true
  else importsAspect aspect subtype

importsAspect :: forall e. ContextID -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
importsAspect aspect = (flip runMonadPerspectivesQuery) (toBoolean (contains aspect aspecten))

mostSpecificCommonAspect :: forall e. Array TypeID -> MonadPerspectives (AjaxAvarCache e) TypeID
mostSpecificCommonAspect types = do
  x <- traverse (runTypedTripleGetter aspecten) types
  aspects <- pure $ join (tripleObjects <$> x)
  foldM (\msca t -> ifM (hasAspect msca t) (pure msca) (pure t)) "model:Perspectives$ElkType" aspects

-- | Either the type of the Rol equals the RolID, or the type has RolID as Aspect. NOTE: rolId represents a type, not a RolInstantie!
typeIsInstanceOfType :: forall e. TypeID -> TypeID -> MonadPerspectives (AjaxAvarCache e) Boolean
typeIsInstanceOfType subType typeId = do
  if typeId == subType then (pure true) else hasAspect typeId subType

-- | Either the type of the Rol equals the RolID, or the type has RolID as Aspect.
-- | `psp:ContextInstance -> psp:ElkType`
typeIsInstanceOfType' :: forall e. TypeID -> TypeID -> MonadPerspectives (AjaxAvarCache e) Boolean
typeIsInstanceOfType' inst typeId = do
  subType <- getContextTypeF inst
  if typeId == subType then (pure true) else hasAspect typeId subType
