module Perpectives.TypeChecker where

import Data.Array (foldM, head, intersect, length, union)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Perspectives.CoreTypes (FD, MonadPerspectives, Triple(..), TypedTripleGetter, UserMessage(..), runMonadPerspectivesQuery, tripleGetter2function, tripleObjects_, (##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, PropertyName, RolName, ID)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI, guardWellFormedNess)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (contains, containsMatching, toBoolean, filter)
import Perspectives.SystemQueries (aspecten, binding, contextRolTypes, mogelijkeBinding, rolPropertyTypes)
import Perspectives.TripleGetter (constructRolGetter)
import Prelude (bind, flip, pure, (&&), (<$>), (<*>), (<<<), (<>), (==), (>>=), (||), ($))

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
alternatives = (constructRolGetter "model:Perspectives$alternative" "model:Perspectives$Sum$alternative") >-> binding

checkContextForQualifiedRol :: forall e. RolName -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
checkContextForQualifiedRol rn cn = do
  aspect <- guardWellFormedNess deconstructLocalNameFromDomeinURI rn
  (&&) <$> checkTypeImportsAspect cn aspect <*> checkContextHasRol aspect rn
  where
    checkContextHasRol :: RolName -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkContextHasRol cn rn = runMonadPerspectivesQuery cn (toBoolean (contains rn contextRolTypes))

checkRolForUnQualifiedProperty :: forall e. PropertyName -> RolName -> MonadPerspectives (AjaxAvarCache e) FD
checkRolForUnQualifiedProperty ln rn = do
  aspects <- aspectsWithUnqualifiedProperty ln rn
  case length aspects of
    0 -> pure $ Left $ MissingUnqualifiedProperty ln rn
    1 -> pure $ Right $ unsafePartial $ fromJust $ head aspects
    otherwise -> pure $ Left $ MultipleDefinitions aspects
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
    otherwise -> pure $ Left $ MultipleDefinitions aspects
  where

    aspectsWithUnqualifiedRol :: MonadPerspectives (AjaxAvarCache e) (Array ID)
    aspectsWithUnqualifiedRol = (cn ## filter (hasUnqualifiedRol ln) aspecten) >>= tripleObjects_

    hasUnqualifiedRol :: PropertyName -> TypedTripleGetter e
    hasUnqualifiedRol ln = containsMatching
      (\contextName rolName -> (contextName <> "$" <> ln) == rolName)
      ("UnqualifiedRol" <> ln)
      contextRolTypes

hasAspect :: forall e. ContextID -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
hasAspect aspect subtype = if aspect == subtype
  then pure true
  else importsAspect aspect subtype

importsAspect :: forall e. ContextID -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
importsAspect aspect = (flip runMonadPerspectivesQuery) (toBoolean (contains aspect aspecten))
