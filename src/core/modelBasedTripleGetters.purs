module Perspectives.ModelBasedTripleGetters where

import Perspectives.CoreTypes (TypedTripleGetter)
import Perspectives.DataTypeTripleGetters (bindingM, iedereRolInContextM, labelM, contextM, buitenRolM)
import Perspectives.ModelBasedObjectGetters (binnenRolBeschrijving, buitenRolBeschrijving, contextDef, propertyIsFunctioneel, propertyIsVerplicht, rolIsFunctioneel, rolIsVerplicht)
import Perspectives.QueryCombinators (closure, closure', filter, notEmpty, concat, containedIn, not, ref) as QC
import Perspectives.TripleGetterComposition ((>->), (>->>))
import Perspectives.TripleGetterConstructors (constructInverseRolGetter, constructRolGetter, constructTripleGetterFromObjectsGetter, rolHasTypeWithLocalName)

-----------------------------------------------------------
-- GETTERS BASED ON MODEL:PERSPECTIVES$
-- These getters are based on properties (defined for roles) and roles (defined for contexts)
-- as modelled in the definitions of CRL and ARC.
-----------------------------------------------------------

-- | True if the Rol has been defined as functional.
-- | `psp:Rol -> psp:Boolean`
rolIsFunctioneelM :: forall e. TypedTripleGetter e
rolIsFunctioneelM = constructTripleGetterFromObjectsGetter "model:Perspectives$Rol$isFunctioneelR" rolIsFunctioneel

-- | True if the Property has been defined as functional.
-- | `psp:Property -> psp:Boolean`
propertyIsFunctioneelM :: forall e. TypedTripleGetter e
propertyIsFunctioneelM = constructTripleGetterFromObjectsGetter "model:Perspectives$Property$isFunctioneelR" propertyIsFunctioneel

-- | True if the Rol has been defined as mandatory.
-- | `psp:Rol -> psp:Boolean`
rolIsVerplichtM :: forall e. TypedTripleGetter e
rolIsVerplichtM = constructTripleGetterFromObjectsGetter "model:Perspectives$Rol$isVerplichtR" rolIsVerplicht

-- | True if the Property has been defined as mandatory (possibly in an aspect).
-- | `psp:Property -> psp:Boolean`
propertyIsVerplichtM :: forall e. TypedTripleGetter e
propertyIsVerplichtM = constructTripleGetterFromObjectsGetter "model:Perspectives$Property$isVerplichtR" propertyIsVerplicht
  -- NOTE. The terminating 'R' distinguishes this triple in the administration of the
  -- 'own' property "model:Perspectives$Property$isVerplicht"

-- | The type of the range that has been defined for the Property.
-- | `psp:Property -> psp:SimpleValueDef`
rangeDefM :: forall e. TypedTripleGetter e
rangeDefM = constructRolGetter "model:Perspectives$Property$range" >-> bindingM >-> contextM

-- | True iff the context instance has a label.
-- | `psp:ContextInstance -> psp:Boolean`
hasLabelM :: forall e. TypedTripleGetter e
hasLabelM = QC.notEmpty labelM

-- | True if the rol instance has a binding.
-- | `psp:RolInstance -> psp:Boolean`
hasBindingM :: forall e. TypedTripleGetter e
hasBindingM = QC.notEmpty bindingM

-- | The role instance at the bottom of the telescope of the rol instance (that role instance will have no binding).
-- | `psp:RolInstance -> psp:RolInstance`
rolUserM :: forall e. TypedTripleGetter e
rolUserM = QC.closure' bindingM

-- | The view that is needed for the object of the Actie.
-- | `psp:Actie -> psp:View`
objectViewDefM :: forall e. TypedTripleGetter e
objectViewDefM = (constructRolGetter "model:Perspectives$Actie$objectView") >-> bindingM >-> contextM

-- | The PropertyReferences of the View.
-- | `psp:View -> psp:PropertyReferentie`
propertyReferentiesM :: forall e. TypedTripleGetter e
propertyReferentiesM = constructRolGetter "model:Perspectives$View$propertyReferentie"

-- | The context instances that are bound to a rol of the context instance.
-- | `psp:ContextInstance -> psp:ContextInstance`
boundContextsM :: forall e. TypedTripleGetter e
boundContextsM = (QC.filter (rolHasTypeWithLocalName "buitenRolBeschrijving") (iedereRolInContextM >-> bindingM)) >-> contextM

-- | All properties defined in namespace of the Rol.
-- | `psp:Rol -> psp:Property`
ownPropertiesDefM :: forall e. TypedTripleGetter e
ownPropertiesDefM = constructRolGetter "model:Perspectives$Rol$rolProperty" >-> bindingM >-> contextM

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
ownRollenDefM = constructRolGetter "model:Perspectives$Context$rolInContext" >-> bindingM >-> contextM

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
ownInternePropertiesDefM = binnenRolBeschrijvingM >-> bindingM >-> contextM >-> propertiesDefM

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
ownExternePropertiesDefM = buitenRolBeschrijvingM >-> bindingM >-> contextM >-> propertiesDefM

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
bindingDefM = constructRolGetter "model:Perspectives$Rol$mogelijkeBinding"  >-> bindingM >-> contextM

-- | The instances of the Rol psp:Context$rolInContext of a context.
-- | `psp:ContextInstance -> psp:RolInstance`
rollenInContextM :: forall e. TypedTripleGetter e
rollenInContextM = constructRolGetter "model:Perspectives$Context$rolInContext"

-- | All direct Aspecten of a Context.
-- | `psp:Context -> psp:Context`
aspectenDefM :: forall e. TypedTripleGetter e
aspectenDefM = constructRolGetter "model:Perspectives$Context$aspect" >-> bindingM >-> contextM

-- | All Rollen from Aspects that have been directly added to a Rol.
-- | `psp:Rol -> psp:Rol`
aspectRollenDefM :: forall e. TypedTripleGetter e
aspectRollenDefM = constructRolGetter "model:Perspectives$Rol$aspectRol" >-> bindingM >-> contextM

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
actiesInContextDefM = constructRolGetter "model:Perspectives$Zaak$actieInContext" >-> bindingM >-> contextM

-- | The Acties that the Rol is the subject (Actor) of.
-- | `psp:Rol -> psp:Actie`
subjectRollenDefM :: forall e. TypedTripleGetter e
subjectRollenDefM = constructRolGetter "model:Perspectives$Rol$subjectRol" >-> bindingM >-> contextM

-- | The Acties that the Rol is the object of.
-- | `psp:Rol -> psp:Actie`
objectRollenDefM :: forall e. TypedTripleGetter e
objectRollenDefM = constructRolGetter "model:Perspectives$Rol$objectRol" >-> bindingM >-> contextM

-- | The Rollen that have this Actie as subjectRol.
-- | `psp:Actie -> psp:Rol`
inverse_subjectRollenDefM :: forall e. TypedTripleGetter e
inverse_subjectRollenDefM = buitenRolM >-> constructInverseRolGetter "model:Perspectives$Rol$subjectRol" >-> contextM

-- | The Acties that the SysteemBot is the subject (Actor) of. Thus, given a SysteemBot, returns its Acties.
-- | `psp:SysteemBot -> psp:Actie`
botSubjectRollenDefM :: forall e. TypedTripleGetter e
botSubjectRollenDefM = constructRolGetter "model:Perspectives$SysteemBot$subjectRol" >-> bindingM >-> contextM

-- | From the description of a Context, return the description of its contextBot (a SysteemBot).
-- | `psp:Context -> psp:SysteemBot`
contextBotDefM :: forall e. TypedTripleGetter e
contextBotDefM = constructRolGetter "model:Perspectives$Context$contextBot"  >-> bindingM >-> contextM

-- | `psp:Rol -> psp:Context`
contextDefM :: forall e. TypedTripleGetter e
contextDefM = constructTripleGetterFromObjectsGetter "model:Perspectives$getContextDef" contextDef

-- | The Context of the RolInContext.
-- | `psp:Rol -> psp:Context`
rolInContextDefM :: forall e. TypedTripleGetter e
rolInContextDefM = buitenRolM >-> constructInverseRolGetter "model:Perspectives$Context$rolInContext" >-> contextM

-- | The Context of the BinnenRol.
-- | `psp:Rol -> psp:Context`
binnenRolContextDefM :: forall e. TypedTripleGetter e
binnenRolContextDefM = buitenRolM >-> constructInverseRolGetter "model:Perspectives$Context$binnenRolBeschrijvingM" >-> contextM

-- | The Context of the BuitenRol.
-- | `psp:Rol -> psp:Context`
buitenRolContextDefM :: forall e. TypedTripleGetter e
buitenRolContextDefM = buitenRolM >-> constructInverseRolGetter "model:Perspectives$Context$buitenRolBeschrijving" >-> contextM

-- | `psp:Context -> psp:RolInstance`
buitenRolBeschrijvingM :: forall e. TypedTripleGetter e
buitenRolBeschrijvingM = constructTripleGetterFromObjectsGetter "model:Perspectives$buitenRolBeschrijving" buitenRolBeschrijving

-- | `psp:Context -> psp:RolInstance`
binnenRolBeschrijvingM :: forall e. TypedTripleGetter e
binnenRolBeschrijvingM = constructTripleGetterFromObjectsGetter "model:Perspectives$binnenRolBeschrijving" binnenRolBeschrijving
