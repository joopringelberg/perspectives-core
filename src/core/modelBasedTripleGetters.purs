module Perspectives.ModelBasedTripleGetters where

import Perspectives.CoreTypes (TypedTripleGetter)
import Perspectives.DataTypeTripleGetters (binding, iedereRolInContext, label, rolContext, buitenRol)
import Perspectives.ModelBasedObjectGetters (getBuitenRolBeschrijving)
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
isFunctionalRol :: forall e. TypedTripleGetter e
isFunctionalRol = constructTripleGetterFromObjectsGetter "model:Perspectives$Rol$isFunctioneelR" Property.rolIsFunctioneel

-- | True if the Property has been defined as functional.
-- | `psp:Property -> psp:Boolean`
isFunctionalProperty :: forall e. TypedTripleGetter e
isFunctionalProperty = constructTripleGetterFromObjectsGetter "model:Perspectives$Property$isFunctioneelR" Property.propertyIsFunctioneel

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
range :: forall e. TypedTripleGetter e
range = constructRolGetter "model:Perspectives$Property$range" >-> binding >-> rolContext

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
objectView :: forall e. TypedTripleGetter e
objectView = (constructRolGetter "model:Perspectives$Actie$objectView") >-> binding >-> rolContext

-- | The PropertyReferences of the View.
-- | `psp:View -> psp:PropertyReferentie`
propertyReferentie :: forall e. TypedTripleGetter e
propertyReferentie = constructRolGetter "model:Perspectives$View$propertyReferentie"

-- | Tests for any rol or context instance if it is a context instance.
-- | NOTE: in a type checked environment, this query is unnecessary.
-- | `psp:ContextInstance | psp:rolInstance -> psp:Boolean`
isContext :: forall e. TypedTripleGetter e
isContext = QC.notEmpty rolContext

-- | The context instances that are bound to a rol of the context instance.
-- | `psp:ContextInstance -> psp:ContextInstance`
boundContexts :: forall e. TypedTripleGetter e
boundContexts = (QC.filter (rolHasTypeWithLocalName "buitenRolBeschrijving") (iedereRolInContext >-> binding)) >-> rolContext

-- | All properties defined in namespace of the Rol.
-- | `psp:Rol -> psp:Property`
rolOwnPropertyTypes :: forall e. TypedTripleGetter e
rolOwnPropertyTypes = constructRolGetter "model:Perspectives$Rol$rolProperty" >-> binding >-> rolContext

-- | All properties derived from Aspects of the Rol.
-- | `psp:Rol -> psp:Property`
rolAspectProperties :: forall e. TypedTripleGetter e
rolAspectProperties = aspectRollen >-> rolOwnPropertyTypes

-- | All properties stored with the rol instance (own and derived from Aspects).
-- | `psp:Rol -> psp:Property`
rolPropertyTypes :: forall e. TypedTripleGetter e
rolPropertyTypes = QC.concat
  rolOwnPropertyTypes
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> rolOwnPropertyTypes)))
    ((aspectRol >->> (\_ -> rolPropertyTypes)) "rolPropertyTypes"))

-- | All Rollen defined for a Context type (excluding Aspects).
-- | `psp:Context -> psp:Rol`
contextOwnRolTypes :: forall e. TypedTripleGetter e
contextOwnRolTypes = constructRolGetter "model:Perspectives$Context$rolInContext" >-> binding >-> rolContext

-- | All Rollen stored with the context instance (own and derived from Aspects).
-- | `psp:Context -> psp:Rol`
contextRolTypes :: forall e. TypedTripleGetter e
contextRolTypes = QC.concat
  contextOwnRolTypes
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> contextOwnRolTypes >-> aspectRol)))
    ((aspect >->> (\_ -> contextRolTypes)) "contextRolTypes"))

-- | All properties defined on the BinnenRol of a Context type.
-- | `psp:Context -> psp:Property`
contextOwnInternePropertyTypes :: forall e. TypedTripleGetter e
contextOwnInternePropertyTypes = constructRolGetter "model:Perspectives$Context$internalProperty" >-> binding >-> rolContext

-- | All properties defined on the BinnenRol of a Context type (own and derived from Aspects).
-- | `psp:Context -> psp:Property`
contextInternePropertyTypes :: forall e. TypedTripleGetter e
contextInternePropertyTypes = QC.concat
  contextOwnInternePropertyTypes
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> contextOwnInternePropertyTypes)))
    ((aspect >->> (\_ -> contextInternePropertyTypes)) "contextInternePropertyTypes"))

-- | All properties defined on the BuitenRol of a Context type.
-- | `psp:Context -> psp:Property`
contextOwnExternePropertyTypes :: forall e. TypedTripleGetter e
-- Neem psp:Context$buitenRol van het contexttype. Neem daarvan de rolProperties (en daarvan de binding en daarvan de rolContext).
contextOwnExternePropertyTypes = buitenRolBeschrijving >-> binding >-> rolContext

-- | All properties defined on the BuitenRol of a Context type (own and derived from Aspects).
-- | `psp:Context -> psp:Property`
contextExternePropertyTypes :: forall e. TypedTripleGetter e
contextExternePropertyTypes = QC.concat
  contextOwnExternePropertyTypes
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> contextOwnInternePropertyTypes)))
    ((aspect >->> (\_ -> contextExternePropertyTypes)) "contextExternePropertyTypes"))

-- | The type of Rol or Context that can be bound to the Rol.
-- | `psp:Rol -> psp:Context | psp:Rol`
mogelijkeBinding :: forall e. TypedTripleGetter e
mogelijkeBinding = constructRolGetter "model:Perspectives$Rol$mogelijkeBinding"  >-> binding >-> rolContext

-- | The instances of the Rol psp:Context$rolInContext of a context.
-- | `psp:ContextInstance -> psp:RolInstance`
rolInContext :: forall e. TypedTripleGetter e
rolInContext = constructRolGetter "model:Perspectives$Context$rolInContext"

-- | All direct Aspecten of a Context.
-- | `psp:Context -> psp:Context`
aspect :: forall e. TypedTripleGetter e
aspect = constructRolGetter "model:Perspectives$Context$aspect" >-> binding >-> rolContext

-- | All Rollen from Aspects that have been directly added to a Rol.
-- | `psp:Rol -> psp:Rol`
aspectRol :: forall e. TypedTripleGetter e
aspectRol = constructRolGetter "model:Perspectives$Rol$aspectRol" >-> binding >-> rolContext

-- | All Rollen from Aspects that have been recursively added to a Rol (the entire inherited hiërarchy).
-- | `psp:Rol -> psp:Rol`
aspectRollen :: forall e. TypedTripleGetter e
aspectRollen = QC.closure aspectRol

-- | All recursively inherited aspects but excluding the subject itself.
-- | `psp:Context -> psp:Context`
aspecten :: forall e. TypedTripleGetter e
aspecten = QC.closure aspect

-- | All RolInstances of a Rol (definition), including those inherited from its rolAspect.
-- | `psp:Rol -> psp:RolInstance`
-- TODO: als we overschrijven toestaan, kunnen hier duplicaten inzitten...
rolTypeRolInstances :: forall e. TypedTripleGetter e
rolTypeRolInstances = QC.concat iedereRolInContext
  ((aspectRol >->> (\_ -> rolTypeRolInstances)) "rolTypeRolInstances")

-- | All acties defined in the Context.
-- | `psp:Context -> psp:Actie`
actieInContext :: forall e. TypedTripleGetter e
actieInContext = constructRolGetter "model:Perspectives$Zaak$actieInContext" >-> binding >-> rolContext

-- | The Acties that the Rol is the subject (Actor) of.
-- | `psp:Rol -> psp:Actie`
subjectRol :: forall e. TypedTripleGetter e
subjectRol = constructRolGetter "model:Perspectives$Rol$subjectRol" >-> binding >-> rolContext

-- | The Acties that the Rol is the object of.
-- | `psp:Rol -> psp:Actie`
objectRol :: forall e. TypedTripleGetter e
objectRol = constructRolGetter "model:Perspectives$Rol$objectRol" >-> binding >-> rolContext

-- | The Rollen that have this Actie as subjectRol.
-- | `psp:Actie -> psp:Rol`
inverse_subjectRol :: forall e. TypedTripleGetter e
inverse_subjectRol = buitenRol >-> constructInverseRolGetter "model:Perspectives$Rol$subjectRol" >-> rolContext

-- | The Context of the Rol.
-- | `psp:Rol -> psp:Context`
contextTypeOfRolType :: forall e. TypedTripleGetter e
contextTypeOfRolType = buitenRol >-> constructInverseRolGetter "model:Perspectives$Context$rolInContext" >-> rolContext

-- | `psp:Context -> psp:RolInstance`
buitenRolBeschrijving :: forall e. TypedTripleGetter e
buitenRolBeschrijving = constructTripleGetterFromObjectsGetter "model:Perspectives$buitenBeschrijving" getBuitenRolBeschrijving
