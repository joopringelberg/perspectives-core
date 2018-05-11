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
    checkRolHasAspect rn' an = (||) <$> an `importsAspect` rn' <*> checkMogelijkeBindingHasAspect rn' an

    checkMogelijkeBindingHasAspect :: RolName -> RolName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkMogelijkeBindingHasAspect rn' an = do
      b <- runMonadPerspectivesQuery rn' (tripleGetter2function mogelijkeBinding)
      case b of
        Nothing -> pure false
        (Just bd) -> do
          (Triple{object} ) <- bd ## alternatives
          case head object of
            Nothing -> checkRolForQualifiedProperty pn bd -- Not a Sum type
            otherwise -> foldM (\r alt -> checkRolForQualifiedProperty pn alt >>= pure <<< (&&) r) true object

    checkRolHasProperty :: RolName -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkRolHasProperty rn' pn' = runMonadPerspectivesQuery rn' (toBoolean (contains pn' rolPropertyTypes))

alternatives :: forall e. TypedTripleGetter e
alternatives = (constructRolGetter "model:Perspectives$Sum$alternative") >-> binding

checkContextForQualifiedRol :: forall e. RolName -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
checkContextForQualifiedRol rn cn = do
  aspect <- guardWellFormedNess deconstructLocalNameFromDomeinURI rn
  (&&) <$> cn `importsAspect` aspect <*> checkContextHasRol aspect rn
  where
    checkContextHasRol :: RolName -> PropertyName -> MonadPerspectives (AjaxAvarCache e) Boolean
    checkContextHasRol cn' rn' = runMonadPerspectivesQuery cn' (toBoolean (contains rn' contextOwnRolTypes))

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

    aspectsWithUnqualifiedProperty :: PropertyName -> RolName -> MonadPerspectives (AjaxAvarCache e) (Array ID)
    aspectsWithUnqualifiedProperty ln' rn = union <$> importedAspectsWithUnqualifiedProperty <*> aspectsFromMogelijkeBindingWithUnqualifiedProperty
      where
        importedAspectsWithUnqualifiedProperty :: MonadPerspectives (AjaxAvarCache e) (Array ID)
        importedAspectsWithUnqualifiedProperty = (rn ## filter (hasUnqualifiedProperty ln') aspecten) >>= tripleObjects_

        aspectsFromMogelijkeBindingWithUnqualifiedProperty :: MonadPerspectives (AjaxAvarCache e) (Array ID)
        aspectsFromMogelijkeBindingWithUnqualifiedProperty = do
          b <- runMonadPerspectivesQuery rn (tripleGetter2function mogelijkeBinding)
          case b of
            Nothing -> pure []
            (Just bd) -> do
              (Triple{object} ) <- bd ## alternatives
              case head object of
                Nothing -> aspectsWithUnqualifiedProperty ln' bd -- Not a Sum type
                otherwise -> foldM (\r alt -> aspectsWithUnqualifiedProperty ln' alt >>= pure <<< intersect r) [] object


    hasUnqualifiedProperty :: PropertyName -> TypedTripleGetter e
    hasUnqualifiedProperty ln' = containsMatching
      (\rolName propertyName -> (rolName <> "$" <> ln') == propertyName)
      ("UnqualifiedProperty" <> ln')
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
    hasUnqualifiedRol ln' = containsMatching
      (\contextName rolName -> (contextName <> "$" <> ln') == rolName)
      ("UnqualifiedRol" <> ln')
      contextOwnRolTypes

-- | True when both parameters are equal and also when the first has the second as aspect:
-- | subtype `isOrHasAspect` aspect
isOrHasAspect :: forall e. ContextID -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
isOrHasAspect subtype aspect = if aspect == subtype
  then pure true
  else subtype `importsAspect` aspect

-- | True iff one of the Aspecten of tp contains the given Aspect.
-- | tp `importsAspect` aspect
-- | Every type has the Aspect psp:ElkType.
importsAspect :: forall e. ContextID -> ContextID -> MonadPerspectives (AjaxAvarCache e) Boolean
importsAspect tp aspect = if aspect == "model:Perspectives$ElkType"
  then pure true
  else (flip runMonadPerspectivesQuery) (toBoolean (contains aspect aspecten)) tp

mostSpecificCommonAspect :: forall e. Array TypeID -> MonadPerspectives (AjaxAvarCache e) TypeID
mostSpecificCommonAspect types = do
  x <- traverse (runTypedTripleGetter aspecten) types
  aspects <- pure $ join (tripleObjects <$> x)
  foldM (\msca t -> ifM (t `isOrHasAspect` msca) (pure msca) (pure t)) "model:Perspectives$ElkType" aspects
