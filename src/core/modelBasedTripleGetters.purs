module Perspectives.ModelBasedTripleGetters where

import Perspectives.CoreTypes (TypedTripleGetter)
import Perspectives.DataTypeTripleGetters (binding, iedereRolInContext, label, context, buitenRol)
import Perspectives.ModelBasedObjectGetters (getBinnenRolBeschrijving, getBuitenRolBeschrijving)
import Perspectives.Property (propertyIsFunctioneel, propertyIsVerplicht, rolIsFunctioneel, rolIsVerplicht) as Property
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
rolIsFunctioneel :: forall e. TypedTripleGetter e
rolIsFunctioneel = constructTripleGetterFromObjectsGetter "model:Perspectives$Rol$isFunctioneelR" Property.rolIsFunctioneel

-- | True if the Property has been defined as functional.
-- | `psp:Property -> psp:Boolean`
propertyIsFunctioneel :: forall e. TypedTripleGetter e
propertyIsFunctioneel = constructTripleGetterFromObjectsGetter "model:Perspectives$Property$isFunctioneelR" Property.propertyIsFunctioneel

-- | True if the Rol has been defined as mandatory.
-- | `psp:Rol -> psp:Boolean`
rolIsVerplicht :: forall e. TypedTripleGetter e
rolIsVerplicht = constructTripleGetterFromObjectsGetter "model:Perspectives$Rol$isVerplichtR" Property.rolIsVerplicht

-- | True if the Property has been defined as mandatory (possibly in an aspect).
-- | `psp:Property -> psp:Boolean`
propertyIsVerplicht :: forall e. TypedTripleGetter e
propertyIsVerplicht = constructTripleGetterFromObjectsGetter "model:Perspectives$Property$isVerplichtR" Property.propertyIsVerplicht
  -- NOTE. The terminating 'R' distinguishes this triple in the administration of the
  -- 'own' property "model:Perspectives$Property$isVerplicht"

-- | The type of the range that has been defined for the Property.
-- | `psp:Property -> psp:SimpleValue`
rangeDef :: forall e. TypedTripleGetter e
rangeDef = constructRolGetter "model:Perspectives$Property$range" >-> binding >-> context

-- | True iff the context instance has a label.
-- | `psp:ContextInstance -> psp:Boolean`
hasLabel :: forall e. TypedTripleGetter e
hasLabel = QC.notEmpty label

-- | True if the rol instance has a binding.
-- | `psp:RolInstance -> psp:Boolean`
hasBinding :: forall e. TypedTripleGetter e
hasBinding = QC.notEmpty binding

-- | The role instance at the bottom of the telescope of the rol instance (that role instance will have no binding).
-- | `psp:RolInstance -> psp:RolInstance`
rolUser :: forall e. TypedTripleGetter e
rolUser = QC.closure' binding

-- | The view that is needed for the object of the Actie.
-- | `psp:Actie -> psp:View`
objectViewDef :: forall e. TypedTripleGetter e
objectViewDef = (constructRolGetter "model:Perspectives$Actie$objectView") >-> binding >-> context

-- | The PropertyReferences of the View.
-- | `psp:View -> psp:PropertyReferentie`
propertyReferentie :: forall e. TypedTripleGetter e
propertyReferentie = constructRolGetter "model:Perspectives$View$propertyReferentie"

-- | The context instances that are bound to a rol of the context instance.
-- | `psp:ContextInstance -> psp:ContextInstance`
boundContexts :: forall e. TypedTripleGetter e
boundContexts = (QC.filter (rolHasTypeWithLocalName "buitenRolBeschrijving") (iedereRolInContext >-> binding)) >-> context

-- | All properties defined in namespace of the Rol.
-- | `psp:Rol -> psp:Property`
ownPropertyDef :: forall e. TypedTripleGetter e
ownPropertyDef = constructRolGetter "model:Perspectives$Rol$rolProperty" >-> binding >-> context

-- | All properties stored with the rol instance (own and derived from Aspects).
-- | `psp:Rol -> psp:Property`
propertyDef :: forall e. TypedTripleGetter e
propertyDef = QC.concat
  ownPropertyDef
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownPropertyDef)))
    ((aspectRolDef >->> (\_ -> propertyDef)) "propertyDef"))

-- | All Rollen defined for a Context type (excluding Aspects).
-- | `psp:Context -> psp:Rol`
ownRolDef :: forall e. TypedTripleGetter e
ownRolDef = constructRolGetter "model:Perspectives$Context$rolInContext" >-> binding >-> context

-- | All Rollen stored with the context instance (own and derived from Aspects).
-- | `psp:Context -> psp:Rol`
rolDef :: forall e. TypedTripleGetter e
rolDef = QC.concat
  ownRolDef
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownRolDef >-> aspectRolDef)))
    ((aspectDef >->> (\_ -> rolDef)) "rolDef"))

-- | All properties defined on the BinnenRol of a Context type.
-- | `psp:Context -> psp:Property`
ownInternePropertyDef :: forall e. TypedTripleGetter e
ownInternePropertyDef = binnenRolBeschrijving >-> binding >-> context >-> propertyDef

-- | All properties defined on the BinnenRol of a Context type (own and derived from Aspects).
-- | `psp:Context -> psp:Property`
internePropertyDef :: forall e. TypedTripleGetter e
internePropertyDef = QC.concat
  ownInternePropertyDef
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownInternePropertyDef)))
    ((aspectDef >->> (\_ -> internePropertyDef)) "internePropertyDef"))

-- | All properties defined on the BuitenRol of a Context type.
-- | `psp:Context -> psp:Property`
ownExternePropertyDef :: forall e. TypedTripleGetter e
-- Neem psp:Context$buitenRol van het contexttype. Neem daarvan de rolProperties (en daarvan de binding en daarvan de context).
ownExternePropertyDef = buitenRolBeschrijving >-> binding >-> context >-> propertyDef

-- | All properties defined on the BuitenRol of a Context type (own and derived from Aspects).
-- | `psp:Context -> psp:Property`
externePropertyDef :: forall e. TypedTripleGetter e
externePropertyDef = QC.concat
  ownExternePropertyDef
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> ownInternePropertyDef)))
    ((aspectDef >->> (\_ -> externePropertyDef)) "externePropertyDef"))

-- | The type of Rol or Context that can be bound to the Rol.
-- | `psp:Rol -> psp:Context | psp:Rol`
bindingDef :: forall e. TypedTripleGetter e
bindingDef = constructRolGetter "model:Perspectives$Rol$mogelijkeBinding"  >-> binding >-> context

-- | The instances of the Rol psp:Context$rolInContext of a context.
-- | `psp:ContextInstance -> psp:RolInstance`
rolInContext :: forall e. TypedTripleGetter e
rolInContext = constructRolGetter "model:Perspectives$Context$rolInContext"

-- | All direct Aspecten of a Context.
-- | `psp:Context -> psp:Context`
aspectDef :: forall e. TypedTripleGetter e
aspectDef = constructRolGetter "model:Perspectives$Context$aspect" >-> binding >-> context

-- | All Rollen from Aspects that have been directly added to a Rol.
-- | `psp:Rol -> psp:Rol`
aspectRolDef :: forall e. TypedTripleGetter e
aspectRolDef = constructRolGetter "model:Perspectives$Rol$aspectRol" >-> binding >-> context

-- | All Rollen from Aspects that have been recursively added to a Rol (the entire inherited hiërarchy).
-- | `psp:Rol -> psp:Rol`
aspectRolDefClosure :: forall e. TypedTripleGetter e
aspectRolDefClosure = QC.closure aspectRolDef

-- | All recursively inherited aspects but excluding the subject itself.
-- | `psp:Context -> psp:Context`
aspectDefClosure :: forall e. TypedTripleGetter e
aspectDefClosure = QC.closure aspectDef

-- TODO. Deze query wordt niet gebruikt. Ik twijfel ook aan zijn betekenis!
-- | All RolInstances of a Rol (definition), including those inherited from its rolAspect.
-- | `psp:Rol -> psp:RolInstance`
-- TODO: als we overschrijven toestaan, kunnen hier duplicaten inzitten...
rolTypeRolInstances :: forall e. TypedTripleGetter e
rolTypeRolInstances = QC.concat iedereRolInContext
  ((aspectRolDef >->> (\_ -> rolTypeRolInstances)) "rolTypeRolInstances")

-- | All acties defined in the Context.
-- | `psp:Context -> psp:Actie`
actieInContextDef :: forall e. TypedTripleGetter e
actieInContextDef = constructRolGetter "model:Perspectives$Zaak$actieInContext" >-> binding >-> context

-- | The Acties that the Rol is the subject (Actor) of.
-- | `psp:Rol -> psp:Actie`
subjectRolDef :: forall e. TypedTripleGetter e
subjectRolDef = constructRolGetter "model:Perspectives$Rol$subjectRol" >-> binding >-> context

-- | The Acties that the Rol is the object of.
-- | `psp:Rol -> psp:Actie`
objectRolDef :: forall e. TypedTripleGetter e
objectRolDef = constructRolGetter "model:Perspectives$Rol$objectRol" >-> binding >-> context

-- | The Rollen that have this Actie as subjectRol.
-- | `psp:Actie -> psp:Rol`
inverse_subjectRolDef :: forall e. TypedTripleGetter e
inverse_subjectRolDef = buitenRol >-> constructInverseRolGetter "model:Perspectives$Rol$subjectRol" >-> context

-- | The Context of the Rol.
-- | `psp:Rol -> psp:Context`
contextDef :: forall e. TypedTripleGetter e
contextDef = buitenRol >-> constructInverseRolGetter "model:Perspectives$Context$rolInContext" >-> context

-- | `psp:Context -> psp:RolInstance`
buitenRolBeschrijving :: forall e. TypedTripleGetter e
buitenRolBeschrijving = constructTripleGetterFromObjectsGetter "model:Perspectives$buitenBeschrijving" getBuitenRolBeschrijving

-- | `psp:Context -> psp:RolInstance`
binnenRolBeschrijving :: forall e. TypedTripleGetter e
binnenRolBeschrijving = constructTripleGetterFromObjectsGetter "model:Perspectives$binnenBeschrijving" getBinnenRolBeschrijving
