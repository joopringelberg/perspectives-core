module Perpectives.TypeChecker where

import Data.Array (foldM, head)
import Data.Maybe (Maybe(..))
import Perspectives.CoreTypes (MonadPerspectives, Triple(..), TypedTripleGetter, runMonadPerspectivesQuery, tripleGetter2function, (##))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, PropertyName, RolName, ID)
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI, guardWellFormedNess)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (contains, toBoolean)
import Perspectives.SystemQueries (aspecten, binding, contextRolTypes, mogelijkeBinding, rolPropertyTypes)
import Perspectives.TripleGetter (constructRolGetter)
import Prelude (bind, pure, (&&), (<$>), (<*>), (>>=), (||), (<<<))

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

      where
      alternatives :: TypedTripleGetter e
      alternatives = (constructRolGetter "model:Perspectives$alternative"
        "model:Perspectives$Sum$alternative") >-> binding

    checkRolHasProperty :: RolName -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkRolHasProperty rn pn = runMonadPerspectivesQuery rn (toBoolean (contains pn rolPropertyTypes))

checkTypeImportsAspect :: forall e. ID -> ID -> MonadPerspectives (AjaxAvarCache e) Boolean
checkTypeImportsAspect typeId an = runMonadPerspectivesQuery typeId (toBoolean (contains an aspecten))

checkContextForQualifiedRol :: forall e. RolName -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
checkContextForQualifiedRol rn cn = do
  aspect <- guardWellFormedNess deconstructLocalNameFromDomeinURI rn
  (&&) <$> checkTypeImportsAspect cn aspect <*> checkContextHasRol aspect rn
  where
    checkContextHasRol :: RolName -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkContextHasRol cn rn = runMonadPerspectivesQuery cn (toBoolean (contains rn contextRolTypes))
