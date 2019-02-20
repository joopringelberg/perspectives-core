module Perspectives.ModelBasedTripleGetters where

import Data.Newtype (unwrap, wrap)
import Perspectives.CoreTypes (TypedTripleGetter, type (**>))
import Perspectives.DataTypeTripleGetters (binding, iedereRolInContext, label, context, buitenRol) as DTG
import Perspectives.ModelBasedObjectGetters (binnenRolBeschrijving, buitenRolBeschrijving, contextDef, propertyIsFunctioneel, propertyIsVerplicht, rolIsFunctioneel, rolIsVerplicht)
import Perspectives.ObjectGetterConstructors (searchContextRol)
import Perspectives.PerspectivesTypes (PBool(..), PropertyDef(..), RolDef(..), SimpleValueDef(..))
import Perspectives.QueryCombinators (closure', filter, notEmpty, containedIn, not, ref) as QC
import Perspectives.TripleGetterComposition (after, before, (>->), (>->>))
import Perspectives.TripleGetterConstructors (closureOfAspectRol, concat, some, searchExternalUnqualifiedProperty)
import Perspectives.TripleGetterFromObjectGetter (constructInverseRolGetter, constructTripleGetterFromObjectsGetter, trackedAs)
import Prelude ((<<<), pure, map)

-----------------------------------------------------------
-- GETTERS BASED ON MODEL:PERSPECTIVES$
-- These getters are based on properties (defined for roles) and roles (defined for contexts)
-- as modelled in the definitions of CRL and ARC.
-----------------------------------------------------------

-- | True if the Rol has been defined as mandatory.
rolIsVerplicht :: forall e. (RolDef **> PBool) e
-- rolIsVerplichtM = rolIsVerplicht `trackedAs` "model:Perspectives$Rol$isVerplichtR"
rolIsVerplicht = some (concat isVerplicht (closureOfAspectRol >-> isVerplicht))
  where
    isVerplicht :: (RolDef **> PBool) e
    isVerplicht = (wrap <<< unwrap) `after` (unwrap `before` (searchExternalUnqualifiedProperty "isVerplicht"))

-- | True if the Rol has been defined as functional.
rolIsFunctioneelM :: forall e. (RolDef **> PBool) e
rolIsFunctioneelM = rolIsFunctioneel `trackedAs` "model:Perspectives$Rol$isFunctioneelR"

-- | True if the Property has been defined as mandatory (possibly in an aspect).
propertyIsVerplichtM :: forall e. (PropertyDef **> PBool) e
propertyIsVerplichtM = propertyIsVerplicht `trackedAs` "model:Perspectives$Property$isVerplicht"

-- | True if the Property has been defined as functional.
propertyIsFunctioneelM :: forall e. (PropertyDef **> PBool) e
propertyIsFunctioneelM = propertyIsFunctioneel `trackedAs` "model:Perspectives$Property$isFunctioneelR"

{-
-- | The type of the range that has been defined for the Property.
-- | `psp:Property -> psp:SimpleValue`
rangeDefM :: forall e. (PropertyDef **> SimpleValueDef) e
rangeDefM = searchContextRol (RolDef "model:Perspectives$Property$range") >-> DTG.binding >-> DTG.context

-- | True iff the context instance has a label.
-- | `psp:ContextInstance -> psp:Boolean`
hasLabelM :: forall e. TypedTripleGetter e
hasLabelM = QC.notEmpty DTG.label

-- | True if the rol instance has a binding.
-- | `psp:RolInstance -> psp:Boolean`
hasBindingM :: forall e. TypedTripleGetter e
hasBindingM = QC.notEmpty DTG.binding

-- | The role instance at the bottom of the telescope of the rol instance (that role instance will have no binding).
-- | `psp:RolInstance -> psp:RolInstance`
rolUserM :: forall e. TypedTripleGetter e
rolUserM = QC.closure' DTG.binding

-- | The view that is needed for the object of the Actie.
-- | `psp:Actie -> psp:View`
objectViewDefM :: forall e. TypedTripleGetter e
objectViewDefM = (constructRolGetter "model:Perspectives$Actie$objectView") >-> DTG.binding >-> DTG.context

-- | The PropertyReferences of the View.
-- | `psp:View -> psp:PropertyReferentie`
propertyReferentiesM :: forall e. TypedTripleGetter e
propertyReferentiesM = constructRolGetter "model:Perspectives$View$propertyReferentie"

-- | The context instances that are bound to a rol of the context instance.
-- | `psp:ContextInstance -> psp:ContextInstance`
boundContextsM :: forall e. TypedTripleGetter e
boundContextsM = (QC.filter (rolHasTypeWithLocalName "buitenRolBeschrijving") (DTG.iedereRolInContext >-> DTG.binding)) >-> DTG.context

-- | All properties defined in namespace of the Rol.
-- | `psp:Rol -> psp:Property`
ownPropertiesDefM :: forall e. TypedTripleGetter e
ownPropertiesDefM = constructRolGetter "model:Perspectives$Rol$rolProperty" >-> DTG.binding >-> DTG.context

-- | All properties stored with the rol instance (own and derived from Aspects).
-- | `psp:Rol -> psp:Property`
propertiesDefM :: forall e. TypedTripleGetter e
propertiesDefM = QC.concat
  ownPropertiesDefM
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownPropertiesDefM)))
    ((aspectRollenDefM >->> (\_ -> propertiesDefM)) "propertyDef"))

-- | All Rollen defined for a Context type, excluding Aspects.
-- | `psp:Context -> psp:Rol`
ownRollenDefM :: forall e. TypedTripleGetter e
ownRollenDefM = constructRolGetter "model:Perspectives$Context$rolInContext" >-> DTG.binding >-> DTG.context

-- | All Rollen defined for a Context type, including Aspects.
-- | `psp:Context -> psp:Rol`
rollenDefM :: forall e. TypedTripleGetter e
rollenDefM = QC.concat
  ownRollenDefM
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownRollenDefM >-> aspectRollenDefM)))
    ((aspectenDefM >->> (\_ -> rollenDefM)) "rolDef"))

-- | All properties defined on the BinnenRol of a Context type.
-- | `psp:Context -> psp:Property`
ownInternePropertiesDefM :: forall e. TypedTripleGetter e
ownInternePropertiesDefM = binnenRolBeschrijvingM >-> DTG.binding >-> DTG.context >-> propertiesDefM

-- | All properties defined on the BinnenRol of a Context type (own and derived from Aspects).
-- | `psp:Context -> psp:Property`
internePropertiesDefM :: forall e. TypedTripleGetter e
internePropertiesDefM = QC.concat
  ownInternePropertiesDefM
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownInternePropertiesDefM)))
    ((aspectenDefM >->> (\_ -> internePropertiesDefM)) "internePropertiesDefM"))

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

-- | The type of Rol or Context that can be bound to the Rol.
-- | `psp:Rol -> psp:Context | psp:Rol`
bindingDefM :: forall e. TypedTripleGetter e
bindingDefM = constructRolGetter "model:Perspectives$Rol$mogelijkeBinding"  >-> DTG.binding >-> DTG.context

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

-- | All acties defined in the Context.
-- | `psp:Context -> psp:Actie`
actiesInContextDefM :: forall e. TypedTripleGetter e
actiesInContextDefM = constructRolGetter "model:Perspectives$Zaak$actieInContext" >-> DTG.binding >-> DTG.context

-- | The Acties that the Rol is the subject (Actor) of.
-- | `psp:Rol -> psp:Actie`
subjectRollenDefM :: forall e. TypedTripleGetter e
subjectRollenDefM = constructRolGetter "model:Perspectives$Rol$subjectRol" >-> DTG.binding >-> DTG.context

-- | The Acties that the Rol is the object of.
-- | `psp:Rol -> psp:Actie`
objectRollenDefM :: forall e. TypedTripleGetter e
objectRollenDefM = constructRolGetter "model:Perspectives$Rol$objectRol" >-> DTG.binding >-> DTG.context

-- | The Rollen that have this Actie as subjectRol.
-- | `psp:Actie -> psp:Rol`
inverse_subjectRollenDefM :: forall e. TypedTripleGetter e
inverse_subjectRollenDefM = DTG.buitenRol >-> constructInverseRolGetter "model:Perspectives$Rol$subjectRol" >-> DTG.context

-- | The Acties that the SysteemBot is the subject (Actor) of. Thus, given a SysteemBot, returns its Acties.
-- | `psp:SysteemBot -> psp:Actie`
botSubjectRollenDefM :: forall e. TypedTripleGetter e
botSubjectRollenDefM = constructRolGetter "model:Perspectives$SysteemBot$subjectRol" >-> DTG.binding >-> DTG.context

-- | From the description of a Context, return the description of its contextBot (a SysteemBot).
-- | `psp:Context -> psp:SysteemBot`
contextBotDefM :: forall e. TypedTripleGetter e
contextBotDefM = constructRolGetter "model:Perspectives$Context$contextBot"  >-> DTG.binding >-> DTG.context

-- | `psp:Rol -> psp:Context`
contextDefM :: forall e. TypedTripleGetter e
contextDefM = constructTripleGetterFromObjectsGetter "model:Perspectives$getContextDef" contextDef

-- | The Context of the RolInContext.
-- | `psp:Rol -> psp:Context`
rolInContextDefM :: forall e. TypedTripleGetter e
rolInContextDefM = DTG.buitenRol >-> constructInverseRolGetter "model:Perspectives$Context$rolInContext" >-> DTG.context

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
