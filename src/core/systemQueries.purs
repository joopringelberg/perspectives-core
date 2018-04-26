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
-- | Both Roles and Properties have the property psp:isFunctional. Hence we model the
-- | domain as psp:Context.
isFunctional :: forall e. TypedTripleGetter e
isFunctional = constructExternalPropertyGetter "model:Perspectives$isFunctional"

isVerplicht :: forall e. TypedTripleGetter e
isVerplicht = constructExternalPropertyGetter "model:Perspectives$isVerplicht"

range :: forall e. TypedTripleGetter e
range = constructExternalPropertyGetter "model:Perspectives$range"

hasLabel :: forall e. TypedTripleGetter e
hasLabel = QC.notEmpty label

hasBinding :: forall e. TypedTripleGetter e
hasBinding = QC.notEmpty binding

rolUser :: forall e. TypedTripleGetter e
rolUser = QC.closure' binding

lijdendVoorwerpBepaling :: forall e. TypedTripleGetter e
lijdendVoorwerpBepaling = (constructRolGetter "model:Perspectives$lijdendVoorwerpBepaling") >-> binding

propertyReferentie :: forall e. TypedTripleGetter e
propertyReferentie = constructRolGetter "model:Perspectives$propertyReferentie"

isContext :: forall e. TypedTripleGetter e
isContext = QC.notEmpty rolContext

boundContexts :: forall e. TypedTripleGetter e
boundContexts = (QC.filter (rolHasType "model:Perspectives$BuitenRol") (iedereRolInContext >-> binding)) >-> rolContext

rolOwnPropertyTypes :: forall e. TypedTripleGetter e
rolOwnPropertyTypes = constructRolGetter "model:Perspectives$rolProperty" >-> binding >-> rolContext

rolAspectProperties :: forall e. TypedTripleGetter e
rolAspectProperties = aspectRollen >-> rolOwnPropertyTypes

rolPropertyTypes :: forall e. TypedTripleGetter e
rolPropertyTypes = QC.concat
  rolOwnPropertyTypes
  (QC.filter (QC.not (QC.containedIn ((QC.ref "#start") >-> rolOwnPropertyTypes)))
    ((aspectRol >->> (\_ -> rolPropertyTypes)) "rolPropertyTypes"))

contextRolTypes :: forall e. TypedTripleGetter e
contextRolTypes = constructRolGetter "model:Perspectives$rolInContext" >-> binding >-> rolContext

contextInternePropertyTypes :: forall e. TypedTripleGetter e
contextInternePropertyTypes = constructRolGetter "model:Perspectives$internalProperty" >-> binding >-> rolContext

contextExternePropertyTypes :: forall e. TypedTripleGetter e
contextExternePropertyTypes = constructRolGetter "model:Perspectives$externalProperty" >-> binding >-> rolContext

mogelijkeBinding :: forall e. TypedTripleGetter e
mogelijkeBinding = constructRolGetter "model:Perspectives$mogelijkeBinding"  >-> binding >-> rolContext

rolInContext :: forall e. TypedTripleGetter e
rolInContext = constructRolGetter "model:Perspectives$rolInContext"

aspect :: forall e. TypedTripleGetter e
aspect = constructRolGetter "model:Perspectives$rolInContext" >-> binding >-> rolContext

aspectRol :: forall e. TypedTripleGetter e
aspectRol = constructRolGetter "model:Perspectives$rolInContext" >-> binding >-> rolContext

aspectRollen :: forall e. TypedTripleGetter e
aspectRollen = QC.closure aspectRol

-- | All aspects but excluding the subject itself.
aspecten :: forall e. TypedTripleGetter e
aspecten = QC.closure aspect
