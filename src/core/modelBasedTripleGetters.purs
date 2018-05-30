module Perspectives.ModelBasedTripleGetters where

import Perspectives.CoreTypes (TypedTripleGetter)
import Perspectives.DataTypeTripleGetters (bindingM, iedereRolInContextM, labelM, contextM, buitenRolM)
import Perspectives.ModelBasedObjectGetters (getBinnenRolBeschrijving, getBuitenRolBeschrijving, getContextDef, propertyIsFunctioneel, propertyIsVerplicht, rolIsFunctioneel, rolIsVerplicht)
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
-- | `psp:Property -> psp:SimpleValue`
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
propertyReferentieM :: forall e. TypedTripleGetter e
propertyReferentieM = constructRolGetter "model:Perspectives$View$propertyReferentie"

-- | The context instances that are bound to a rol of the context instance.
-- | `psp:ContextInstance -> psp:ContextInstance`
boundContextsM :: forall e. TypedTripleGetter e
boundContextsM = (QC.filter (rolHasTypeWithLocalName "buitenRolBeschrijving") (iedereRolInContextM >-> bindingM)) >-> contextM

-- | All properties defined in namespace of the Rol.
-- | `psp:Rol -> psp:Property`
ownPropertyDefM :: forall e. TypedTripleGetter e
ownPropertyDefM = constructRolGetter "model:Perspectives$Rol$rolProperty" >-> bindingM >-> contextM

-- | All properties stored with the rol instance (own and derived from Aspects).
-- | `psp:Rol -> psp:Property`
propertyDefM :: forall e. TypedTripleGetter e
propertyDefM = QC.concat
  ownPropertyDefM
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownPropertyDefM)))
    ((aspectRolDefM >->> (\_ -> propertyDefM)) "propertyDef"))

-- | All Rollen defined for a Context type, excluding Aspects.
-- | `psp:Context -> psp:Rol`
ownRolDefM :: forall e. TypedTripleGetter e
ownRolDefM = constructRolGetter "model:Perspectives$Context$rolInContext" >-> bindingM >-> contextM

-- | All Rollen defined for a Context type, including Aspects.
-- | `psp:Context -> psp:Rol`
rolDefM :: forall e. TypedTripleGetter e
rolDefM = QC.concat
  ownRolDefM
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownRolDefM >-> aspectRolDefM)))
    ((aspectDefM >->> (\_ -> rolDefM)) "rolDef"))

-- | All properties defined on the BinnenRol of a Context type.
-- | `psp:Context -> psp:Property`
ownInternePropertyDefM :: forall e. TypedTripleGetter e
ownInternePropertyDefM = binnenRolBeschrijving >-> bindingM >-> contextM >-> propertyDefM

-- | All properties defined on the BinnenRol of a Context type (own and derived from Aspects).
-- | `psp:Context -> psp:Property`
internePropertyDefM :: forall e. TypedTripleGetter e
internePropertyDefM = QC.concat
  ownInternePropertyDefM
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownInternePropertyDefM)))
    ((aspectDefM >->> (\_ -> internePropertyDefM)) "internePropertyDefM"))

-- | All properties defined on the BuitenRol of a Context type.
-- | `psp:Context -> psp:Property`
ownExternePropertyDefM :: forall e. TypedTripleGetter e
-- Neem psp:Context$buitenRol van het contexttype. Neem daarvan de rolProperties (en daarvan de binding en daarvan de context).
ownExternePropertyDefM = buitenRolBeschrijving >-> bindingM >-> contextM >-> propertyDefM

-- | All properties defined on the BuitenRol of a Context type (own and derived from Aspects).
-- | `psp:Context -> psp:Property`
externePropertyDefM :: forall e. TypedTripleGetter e
externePropertyDefM = QC.concat
  ownExternePropertyDefM
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownInternePropertyDefM)))
    ((aspectDefM >->> (\_ -> externePropertyDefM)) "externePropertyDefM"))

-- | The type of Rol or Context that can be bound to the Rol.
-- | `psp:Rol -> psp:Context | psp:Rol`
bindingDefM :: forall e. TypedTripleGetter e
bindingDefM = constructRolGetter "model:Perspectives$Rol$mogelijkeBinding"  >-> bindingM >-> contextM

-- | The instances of the Rol psp:Context$rolInContext of a context.
-- | `psp:ContextInstance -> psp:RolInstance`
rolInContextM :: forall e. TypedTripleGetter e
rolInContextM = constructRolGetter "model:Perspectives$Context$rolInContext"

-- | All direct Aspecten of a Context.
-- | `psp:Context -> psp:Context`
aspectDefM :: forall e. TypedTripleGetter e
aspectDefM = constructRolGetter "model:Perspectives$Context$aspect" >-> bindingM >-> contextM

-- | All Rollen from Aspects that have been directly added to a Rol.
-- | `psp:Rol -> psp:Rol`
aspectRolDefM :: forall e. TypedTripleGetter e
aspectRolDefM = constructRolGetter "model:Perspectives$Rol$aspectRol" >-> bindingM >-> contextM

-- | All Rollen from Aspects that have been recursively added to a Rol (the entire inherited hiërarchy).
-- | `psp:Rol -> psp:Rol`
aspectRolDefClosure :: forall e. TypedTripleGetter e
aspectRolDefClosure = QC.closure aspectRolDefM

-- | All recursively inherited aspects but excluding the subject itself.
-- | `psp:Context -> psp:Context`
aspectDefClosure :: forall e. TypedTripleGetter e
aspectDefClosure = QC.closure aspectDefM

-- TODO. Deze query wordt niet gebruikt. Ik twijfel ook aan zijn betekenis!
-- | All RolInstances of a Rol (definition), including those inherited from its rolAspect.
-- | `psp:Rol -> psp:RolInstance`
-- TODO: als we overschrijven toestaan, kunnen hier duplicaten inzitten...
rolTypeRolInstances :: forall e. TypedTripleGetter e
rolTypeRolInstances = QC.concat iedereRolInContextM
  ((aspectRolDefM >->> (\_ -> rolTypeRolInstances)) "rolTypeRolInstances")

-- | All acties defined in the Context.
-- | `psp:Context -> psp:Actie`
actieInContextDef :: forall e. TypedTripleGetter e
actieInContextDef = constructRolGetter "model:Perspectives$Zaak$actieInContext" >-> bindingM >-> contextM

-- | The Acties that the Rol is the subject (Actor) of.
-- | `psp:Rol -> psp:Actie`
subjectRolDef :: forall e. TypedTripleGetter e
subjectRolDef = constructRolGetter "model:Perspectives$Rol$subjectRol" >-> bindingM >-> contextM

-- | The Acties that the Rol is the object of.
-- | `psp:Rol -> psp:Actie`
objectRolDef :: forall e. TypedTripleGetter e
objectRolDef = constructRolGetter "model:Perspectives$Rol$objectRol" >-> bindingM >-> contextM

-- | The Rollen that have this Actie as subjectRol.
-- | `psp:Actie -> psp:Rol`
inverse_subjectRolDef :: forall e. TypedTripleGetter e
inverse_subjectRolDef = buitenRolM >-> constructInverseRolGetter "model:Perspectives$Rol$subjectRol" >-> contextM

-- | `psp:Rol -> psp:Context`
contextDef :: forall e. TypedTripleGetter e
contextDef = constructTripleGetterFromObjectsGetter "model:Perspectives$getContextDef" getContextDef

-- | The Context of the RolInContext.
-- | `psp:Rol -> psp:Context`
rolInContextDef :: forall e. TypedTripleGetter e
rolInContextDef = buitenRolM >-> constructInverseRolGetter "model:Perspectives$Context$rolInContext" >-> contextM

-- | The Context of the BinnenRol.
-- | `psp:Rol -> psp:Context`
binnenRolContextDef :: forall e. TypedTripleGetter e
binnenRolContextDef = buitenRolM >-> constructInverseRolGetter "model:Perspectives$Context$binnenRolBeschrijving" >-> contextM

-- | The Context of the BuitenRol.
-- | `psp:Rol -> psp:Context`
buitenRolContextDef :: forall e. TypedTripleGetter e
buitenRolContextDef = buitenRolM >-> constructInverseRolGetter "model:Perspectives$Context$buitenRolBeschrijving" >-> contextM

-- | `psp:Context -> psp:RolInstance`
buitenRolBeschrijving :: forall e. TypedTripleGetter e
buitenRolBeschrijving = constructTripleGetterFromObjectsGetter "model:Perspectives$buitenRolBeschrijving" getBuitenRolBeschrijving

-- | `psp:Context -> psp:RolInstance`
binnenRolBeschrijving :: forall e. TypedTripleGetter e
binnenRolBeschrijving = constructTripleGetterFromObjectsGetter "model:Perspectives$binnenRolBeschrijving" getBinnenRolBeschrijving
