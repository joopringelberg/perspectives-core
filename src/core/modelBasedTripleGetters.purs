module Perspectives.ModelBasedTripleGetters where

import Data.Foldable (foldMap)
import Data.Maybe (maybe)
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF, unwrap, wrap)
import Perspectives.CoreTypes (TypedTripleGetter, type (**>))
import Perspectives.DataTypeObjectGetters (rolType)
import Perspectives.DataTypeTripleGetters (binding, iedereRolInContext, label, context, genericBinding, rolBindingDef, buitenRol) as DTG
import Perspectives.DataTypeTripleGetters (rolBindingDef)
import Perspectives.Identifiers (LocalName) as ID
import Perspectives.Identifiers (deconstructLocalNameFromDomeinURI)
import Perspectives.ModelBasedObjectGetters (buitenRolBeschrijvingDef, binnenRolBeschrijvingDef) as MBOG
import Perspectives.ObjectsGetterComposition (composeMonoidal)
import Perspectives.PerspectivesTypes (class RolClass, ActieDef, AnyContext, AnyDefinition, ContextDef(..), ContextRol(..), PBool(..), PropertyDef(..), RolDef(..), RolInContext(..), SimpleValueDef(..), UserRolDef, ZaakDef, typeWithPerspectivesTypes)
import Perspectives.QueryCombinators (closure', filter, notEmpty) as QC
import Perspectives.StringTripleGetterConstructors (searchInAspectsAndPrototypes)
import Perspectives.TripleGetterComposition (before, composeLazy, followedBy, (>->), (>->>))
import Perspectives.TripleGetterConstructors (closureOfAspectProperty, closureOfAspectRol, concat, searchContextRol, searchExternalUnqualifiedProperty, searchRolInContext, searchUnqualifiedRolDefinition, some, getContextRol)
import Perspectives.TripleGetterFromObjectGetter (constructInverseRolGetter, trackedAs)
import Prelude (show, (<<<), (<>), (==), (>>>), ($))

-----------------------------------------------------------
-- GETTERS BASED ON MODEL:PERSPECTIVES$
-- These getters are based on properties (defined for roles) and roles (defined for contexts)
-- as modelled in the definitions of CRL and ARC.
-----------------------------------------------------------

-- | True if the Rol has been defined as mandatory.
rolIsVerplicht :: forall e. (RolDef **> PBool) e
rolIsVerplicht = some (concat isVerplicht (closureOfAspectRol >-> isVerplicht))
  where
    isVerplicht :: (RolDef **> PBool) e
    isVerplicht = (unwrap `before` (searchExternalUnqualifiedProperty "isVerplicht")) `followedBy`(wrap <<< unwrap)

-- | True if the Rol has been defined as functional.
rolIsFunctioneel :: forall e. (RolDef **> PBool) e
rolIsFunctioneel = some (concat isFunctioneel (closureOfAspectRol >-> isFunctioneel))
  where
    isFunctioneel :: (RolDef **> PBool) e
    isFunctioneel = (unwrap `before` (searchExternalUnqualifiedProperty "isFunctioneel")) `followedBy` (wrap <<< unwrap)

-- | True if the Property has been defined as mandatory (possibly in an aspect).
propertyIsVerplicht :: forall e. (PropertyDef **> PBool) e
propertyIsVerplicht = some (concat isVerplicht (closureOfAspectProperty >-> isVerplicht))
  where
    isVerplicht :: (PropertyDef **> PBool) e
    isVerplicht = (unwrap `before` (searchExternalUnqualifiedProperty "isVerplicht")) `followedBy` (wrap <<< unwrap)

-- | True if the Property has been defined as functional.
propertyIsFunctioneel :: forall e. (PropertyDef **> PBool) e
propertyIsFunctioneel = some (concat isFunctioneel (closureOfAspectProperty >-> isFunctioneel))
  where
    isFunctioneel :: (PropertyDef **> PBool) e
    isFunctioneel = (unwrap `before` (searchExternalUnqualifiedProperty "isFunctioneel")) `followedBy` (wrap <<< unwrap)

rangeDef :: forall e. (PropertyDef **> SimpleValueDef) e
rangeDef = (unwrap `before` (searchContextRol (RolDef "model:Perspectives$Property$range") >-> DTG.binding >-> DTG.context)) `followedBy` SimpleValueDef

-- | True iff the context instance has a label.
hasLabel :: forall e. (AnyContext **> PBool) e
hasLabel = QC.notEmpty DTG.label

-- | True if the rol instance has a binding.
hasBinding :: forall r e. RolClass r => (r **> PBool) e
hasBinding = QC.notEmpty (unwrap `before` DTG.genericBinding)

-- | The role instance at the bottom of the telescope of the rol instance (that role instance will have no binding).
rolUser :: forall e. (RolInContext **> RolInContext) e
rolUser = QC.closure' DTG.binding

-- | The view that is needed for the object of the Actie. Notice that the typing is not precise: instead of Action we
-- use ContextDef, instead of View we use Rol.
-- | That is not correct, but we do not have a separate types for Action, nor View.
objectViewDef :: forall e. (ContextDef **> RolDef) e
objectViewDef = searchUnqualifiedRolDefinition "objectView"

-- | The PropertyReferences of the View. Again, the typing is imprecise.
propertyReferenties :: forall e. (RolDef **> ContextRol) e
propertyReferenties = typeWithPerspectivesTypes searchUnqualifiedRolDefinition "propertyReferentie"

-- | Tests whether the type of the Rol has a specific local name. Used to test if a Rol is a BuitenRol type or a BinnenRol type.
-- | `psp:Rol -> psp:RolInstance -> psp:Boolean`
rolHasTypeWithLocalName :: forall e. ID.LocalName -> TypedTripleGetter String PBool e
rolHasTypeWithLocalName localName = ((RolInContext >>> rolType) `composeMonoidal` f) `trackedAs` ("model:Perspectives$rolHasTypeWithLocalName" <> "_" <> localName)
  where
    f :: Array RolDef -> PBool
    f = PBool <<< show <<< alaF Disj foldMap (maybe false ((==) localName) <<< deconstructLocalNameFromDomeinURI <<< unwrap)

-- | The context instances that are bound to a rol of the context instance.
boundContexts :: forall e. (AnyContext **> ContextDef) e
boundContexts = (QC.filter (rolHasTypeWithLocalName "buitenRolBeschrijving") (DTG.iedereRolInContext >-> DTG.genericBinding)) >-> ContextRol `before` DTG.context `followedBy` ContextDef

-- | The Acties that the Rol is the subject (Actor) of.
-- | `psp:Rol -> psp:Actie`
actiesOfRol :: forall e. (RolDef **> ActieDef) e
actiesOfRol = unwrap `before` (searchRolInContext (RolDef "model:Perspectives$Rol$subjectRol")) >-> DTG.rolBindingDef `followedBy` ContextDef

-- | Move to the enclosing definition of the definition by reversing over $rolInContext.
-- | `psp:Rol -> psp:Context`
enclosingDefinition :: forall e. (AnyDefinition **> AnyDefinition) e
enclosingDefinition = DTG.buitenRol >-> constructInverseRolGetter (RolDef "model:Perspectives$Context$rolInContext") >-> (DTG.context :: (RolInContext **> AnyDefinition) e)

-- | All acties defined in the Context.
-- | `psp:Context -> psp:Actie`
actiesInContextDef :: forall e. (ZaakDef **> ActieDef) e
actiesInContextDef = unwrap `before` searchRolInContext (RolDef "model:Perspectives$Zaak$actieInContext") >-> DTG.binding >-> DTG.context `followedBy` ContextDef

-- | The Acties that the Rol is the object of.
objectRollenDef :: forall e. (RolDef **> ActieDef) e
objectRollenDef = unwrap `before` searchRolInContext (RolDef "model:Perspectives$Rol$objectRol") >-> DTG.binding >-> DTG.context `followedBy` ContextDef

-- | The Rollen that have this Actie as subjectRol.
-- | `psp:Actie -> psp:Rol`
inverse_subjectRollenDef :: forall e. (ActieDef **> UserRolDef) e
inverse_subjectRollenDef = unwrap `before` DTG.buitenRol >-> constructInverseRolGetter (RolDef "model:Perspectives$Rol$subjectRol") >-> (DTG.context :: (RolInContext **> AnyContext)e) `followedBy` RolDef

-- | The type of Rol or Context that can be bound to the Rol.
-- | `psp:Rol -> psp:Context | psp:Rol`
mogelijkeBinding :: forall e. (RolDef **> AnyDefinition) e
mogelijkeBinding = unwrap `before` searchRolInContext (RolDef "model:Perspectives$Rol$mogelijkeBinding")  >-> DTG.binding >-> DTG.context

-- | All Rollen defined for a Context type, excluding Aspects.
-- | `psp:Context -> psp:Rol`
ownRollenDef :: forall e. (AnyContext **> RolDef) e
ownRollenDef = getContextRol (RolDef "model:Perspectives$Context$rolInContext") >-> DTG.binding >-> DTG.context `followedBy` RolDef

rollenDef :: forall e. (AnyContext **> RolDef) e
rollenDef = searchInAspectsAndPrototypes (getContextRol (RolDef "model:Perspectives$Context$rolInContext") >-> DTG.binding >-> DTG.context)  `followedBy` RolDef

-- | All properties defined in the namespace of the Rol.
ownPropertiesDef :: forall e. (RolDef **> PropertyDef) e
ownPropertiesDef = unwrap `before` (getContextRol $ RolDef "model:Perspectives$Rol$rolProperty") >-> DTG.binding >-> DTG.context `followedBy` PropertyDef

-- | All defined for the Rol, in the same namespace, on aspects, or on the MogelijkeBinding.
propertiesDef :: forall e. (RolDef **> PropertyDef) e
propertiesDef = concat defsInAspectsAndPrototypes defsInMogelijkeBinding where

  defsInAspectsAndPrototypes :: (RolDef **> PropertyDef) e
  defsInAspectsAndPrototypes = (unwrap `before`
    (searchInAspectsAndPrototypes (RolDef `before` ownPropertiesDef `followedBy` unwrap))
    `followedBy` PropertyDef)

  defsInMogelijkeBinding :: (RolDef **> PropertyDef) e
  defsInMogelijkeBinding = composeLazy
    (mogelijkeBinding `followedBy` RolDef)
    (\_ -> propertiesDef)
    "propertiesDef"

buitenRolBeschrijvingDef :: forall e. (AnyDefinition **> RolDef) e
buitenRolBeschrijvingDef = MBOG.buitenRolBeschrijvingDef `trackedAs` "buitenRolBeschrijving"

binnenRolBeschrijvingDef :: forall e. (AnyDefinition **> RolDef) e
binnenRolBeschrijvingDef = MBOG.binnenRolBeschrijvingDef `trackedAs` "buitenRolBeschrijving"

-- | From the description of a Context, return the description of its contextBot (a SysteemBot).
contextBotDef :: forall e. (AnyContext **> AnyContext)e
contextBotDef = getContextRol (RolDef "model:Perspectives$Context$contextBot")  >-> DTG.binding >-> DTG.context

-- | The Acties that the SysteemBot is the subject (Actor) of. Thus, given a SysteemBot, returns its Acties.
-- | `psp:SysteemBot -> psp:Actie`
botSubjectRollenDef :: forall e. (AnyContext **> AnyContext) e
botSubjectRollenDef = getContextRol (RolDef "model:Perspectives$SysteemBot$subjectRol") >-> DTG.binding >-> DTG.context


-- propertiesDefM = QC.concat
--   ownPropertiesDefM
--   (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownPropertiesDefM)))
--     ((aspectRollenDefM >->> (\_ -> propertiesDefM)) "propertyDef"))
--
--
-- -- | All properties defined on the BinnenRol of a Context type (own and derived from Aspects).
-- -- | `psp:Context -> psp:Property`
-- internePropertiesDefM :: forall e. TypedTripleGetter e
-- internePropertiesDefM = QC.concat
--   ownInternePropertiesDefM
--   (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownInternePropertiesDefM)))
--     ((aspectenDefM >->> (\_ -> internePropertiesDefM)) "internePropertiesDefM"))
--
-- -- | All properties defined on the BinnenRol of a Context type.
-- -- | `psp:Context -> psp:Property`
-- ownInternePropertiesDefM :: forall e. TypedTripleGetter e
-- ownInternePropertiesDefM = binnenRolBeschrijvingM >-> DTG.binding >-> DTG.context >-> propertiesDefM


{-
-- | All Rollen defined for a Context type, including Aspects.
-- | `psp:Context -> psp:Rol`
rollenDefM :: forall e. TypedTripleGetter e
rollenDefM = QC.concat
  ownRollenDefM
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownRollenDefM >-> aspectRollenDefM)))
    ((aspectenDefM >->> (\_ -> rollenDefM)) "rolDef"))


-- | All properties defined on the BuitenRol of a Context type.
-- | `psp:Context -> psp:Property`
ownExternePropertiesDefM :: forall e. TypedTripleGetter e
-- Neem psp:Context$buitenRol van het contexttype. Neem daarvan de rolProperties (en daarvan de binding en daarvan de context).
ownExternePropertiesDefM = buitenRolBeschrijvingM >-> DTG.binding >-> DTG.context >-> propertiesDefM

-- | All properties defined on the BuitenRol of a Context type (own and derived from Aspects).
-- | `psp:Context -> psp:Property`
externePropertiesDefM :: forall e. TypedTripleGetter e
externePropertiesDefM = QC.concat
  ownExternePropertiesDefM
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownInternePropertiesDefM)))
    ((aspectenDefM >->> (\_ -> externePropertiesDefM)) "externePropertiesDefM"))

-- | The instances of the Rol psp:Context$rolInContext of a context.
-- | `psp:ContextInstance -> psp:RolInstance`
rollenInContextM :: forall e. TypedTripleGetter e
rollenInContextM = constructRolGetter "model:Perspectives$Context$rolInContext"

-- | All direct Aspecten of a Context.
-- | `psp:Context -> psp:Context`
aspectenDefM :: forall e. TypedTripleGetter e
aspectenDefM = constructRolGetter "model:Perspectives$Context$aspect" >-> DTG.binding >-> DTG.context

-- | All Rollen from Aspects that have been directly added to a Rol.
-- | `psp:Rol -> psp:Rol`
aspectRollenDefM :: forall e. TypedTripleGetter e
aspectRollenDefM = constructRolGetter "model:Perspectives$Rol$aspectRol" >-> DTG.binding >-> DTG.context

-- | All Rollen from Aspects that have been recursively added to a Rol (the entire inherited hiërarchy).
-- | `psp:Rol -> psp:Rol`
aspectRollenDefMClosure :: forall e. TypedTripleGetter e
aspectRollenDefMClosure = QC.closure aspectRollenDefM

-- | All recursively inherited aspects but excluding the subject itself.
-- | `psp:Context -> psp:Context`
aspectenDefMClosure :: forall e. TypedTripleGetter e
aspectenDefMClosure = QC.closure aspectenDefM

-- | `psp:Rol -> psp:Context`
contextDefM :: forall e. TypedTripleGetter e
contextDefM = constructTripleGetterFromObjectsGetter "model:Perspectives$getContextDef" contextDef

-- | The Context of the BinnenRol.
-- | `psp:Rol -> psp:Context`
binnenRolContextDefM :: forall e. TypedTripleGetter e
binnenRolContextDefM = DTG.buitenRol >-> constructInverseRolGetter "model:Perspectives$Context$binnenRolBeschrijvingM" >-> DTG.context

-- | The Context of the BuitenRol.
-- | `psp:Rol -> psp:Context`
buitenRolContextDefM :: forall e. TypedTripleGetter e
buitenRolContextDefM = DTG.buitenRol >-> constructInverseRolGetter "model:Perspectives$Context$buitenRolBeschrijving" >-> DTG.context

-- | `psp:Context -> psp:RolInstance`
buitenRolBeschrijvingM :: forall e. TypedTripleGetter e
buitenRolBeschrijvingM = constructTripleGetterFromObjectsGetter "model:Perspectives$buitenRolBeschrijving" buitenRolBeschrijving

-- | `psp:Context -> psp:RolInstance`
binnenRolBeschrijvingM :: forall e. TypedTripleGetter e
binnenRolBeschrijvingM = constructTripleGetterFromObjectsGetter "model:Perspectives$binnenRolBeschrijving" binnenRolBeschrijving
-}
