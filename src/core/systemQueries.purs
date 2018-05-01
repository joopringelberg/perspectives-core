module Perspectives.SystemQueries where

import Data.Array (elemIndex)
import Data.Maybe (maybe)
import Perspectives.CoreTypes (ObjectsGetter, TypedTripleGetter)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Property (getBuitenRol, getContextType, getDisplayName, getRolBinding, getRolContext, getRolType, getRolTypen, getRollen)
import Perspectives.PropertyComposition ((>->), (>->>))
import Perspectives.QueryCombinators (closure, closure', filter, notEmpty, concat, containedIn, not, ref) as QC
import Perspectives.TripleGetter (constructExternalPropertyGetter, constructRolGetter, constructTripleGetterFromObjectsGetter)
import Prelude (const, pure, (<>), (>=>))

-----------------------------------------------------------
-- SYSTEM GETTERS
-- These getters are defined on other members of PerspectRol and PerspectContext than
-- rolInContext (PerspectContext) or properties (PerspectRol). All are memorizing.
-----------------------------------------------------------

identity' :: forall e. ObjectsGetter e
identity' x = pure [x]

identity :: forall e. TypedTripleGetter e
identity = constructTripleGetterFromObjectsGetter "model:Perspectives$identity" identity'

contextType :: forall e. TypedTripleGetter e
contextType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getContextType

buitenRol :: forall e. TypedTripleGetter e
buitenRol = constructTripleGetterFromObjectsGetter "model:Perspectives$buitenRol" getBuitenRol

iedereRolInContext :: forall e. TypedTripleGetter e
iedereRolInContext =  constructTripleGetterFromObjectsGetter "model:Perspectives$iedereRolInContext" getRollen

-- | The names of every rol given to this context.
rolTypen :: forall e. TypedTripleGetter e
rolTypen =  constructTripleGetterFromObjectsGetter "model:Perspectives$rolTypen" getRolTypen

rolType :: forall e. TypedTripleGetter e
rolType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getRolType

binding :: forall e. TypedTripleGetter e
binding = constructTripleGetterFromObjectsGetter "model:Perspectives$binding" getRolBinding

rolContext :: forall e. TypedTripleGetter e
rolContext = constructTripleGetterFromObjectsGetter "model:Perspectives$context" getRolContext

label :: forall e. TypedTripleGetter e
label = constructTripleGetterFromObjectsGetter "model:Perspectives$label" getDisplayName

rolHasType :: forall e. ID -> TypedTripleGetter e
rolHasType typeId = constructTripleGetterFromObjectsGetter ("model:Perspectives$rolHasType" <> "_" <> typeId)
  (getRolType >=> \(objs::Array String) -> pure (maybe ["false"] (const ["true"]) (elemIndex typeId objs)))

-----------------------------------------------------------
-- GETTERS BASED ON MODEL:PERSPECTIVES$
-- These getters are based on properties (defined for roles) and roles (defined for contexts)
-- as modelled in the definitions of CRL and ARC.
-----------------------------------------------------------
isFunctionalRol :: forall e. TypedTripleGetter e
isFunctionalRol = constructExternalPropertyGetter "model:Perspectives$Rol$isFunctional"

isFunctionalProperty :: forall e. TypedTripleGetter e
isFunctionalProperty = constructExternalPropertyGetter "model:Perspectives$Property$isFunctional"

rolIsVerplicht :: forall e. TypedTripleGetter e
rolIsVerplicht = constructExternalPropertyGetter "model:Perspectives$Rol$isVerplicht"

propertyIsVerplicht :: forall e. TypedTripleGetter e
propertyIsVerplicht = constructExternalPropertyGetter "model:Perspectives$Property$isVerplicht"

range :: forall e. TypedTripleGetter e
range = constructExternalPropertyGetter "model:Perspectives$Property$range"

hasLabel :: forall e. TypedTripleGetter e
hasLabel = QC.notEmpty label

hasBinding :: forall e. TypedTripleGetter e
hasBinding = QC.notEmpty binding

rolUser :: forall e. TypedTripleGetter e
rolUser = QC.closure' binding

objectView :: forall e. TypedTripleGetter e
objectView = (constructRolGetter "model:Perspectives$Actie$objectView") >-> binding

propertyReferentie :: forall e. TypedTripleGetter e
propertyReferentie = constructRolGetter "model:Perspectives$View$propertyReferentie"

isContext :: forall e. TypedTripleGetter e
isContext = QC.notEmpty rolContext

boundContexts :: forall e. TypedTripleGetter e
boundContexts = (QC.filter (rolHasType "model:Perspectives$BuitenRol") (iedereRolInContext >-> binding)) >-> rolContext

rolOwnPropertyTypes :: forall e. TypedTripleGetter e
rolOwnPropertyTypes = constructRolGetter "model:Perspectives$Rol$rolProperty" >-> binding >-> rolContext

rolAspectProperties :: forall e. TypedTripleGetter e
rolAspectProperties = aspectRollen >-> rolOwnPropertyTypes

rolPropertyTypes :: forall e. TypedTripleGetter e
rolPropertyTypes = QC.concat
  rolOwnPropertyTypes
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> rolOwnPropertyTypes)))
    ((aspectRol >->> (\_ -> rolPropertyTypes)) "rolPropertyTypes"))

contextRolTypes :: forall e. TypedTripleGetter e
contextRolTypes = constructRolGetter "model:Perspectives$Context$rolInContext" >-> binding >-> rolContext

contextInternePropertyTypes :: forall e. TypedTripleGetter e
contextInternePropertyTypes = constructRolGetter "model:Perspectives$Context$internalProperty" >-> binding >-> rolContext

contextExternePropertyTypes :: forall e. TypedTripleGetter e
contextExternePropertyTypes = constructRolGetter "model:Perspectives$Context$externalProperty" >-> binding >-> rolContext

mogelijkeBinding :: forall e. TypedTripleGetter e
mogelijkeBinding = constructRolGetter "model:Perspectives$Rol$mogelijkeBinding"  >-> binding >-> rolContext

rolInContext :: forall e. TypedTripleGetter e
rolInContext = constructRolGetter "model:Perspectives$Context$rolInContext"

aspect :: forall e. TypedTripleGetter e
aspect = constructRolGetter "model:Perspectives$Context$aspect" >-> binding >-> rolContext

aspectRol :: forall e. TypedTripleGetter e
aspectRol = constructRolGetter "model:Perspectives$Rol$aspectRol" >-> binding >-> rolContext

aspectRollen :: forall e. TypedTripleGetter e
aspectRollen = QC.closure aspectRol

-- | All aspects but excluding the subject itself.
aspecten :: forall e. TypedTripleGetter e
aspecten = QC.closure aspect
