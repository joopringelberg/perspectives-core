module Perspectives.SystemQueries where

import Data.Array (elemIndex)
import Data.Maybe (maybe)
import Perspectives.CoreTypes (ObjectsGetter, TypedTripleGetter)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Property (getBuitenRol, getContextType, getDisplayName, getRolBinding, getRolContext, getRolType, getRolTypen, getRollen)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (closure, closure', filter, notEmpty) as QC
import Perspectives.TripleGetter (constructExternalPropertyGetter, constructRolGetter, constructTripleGetterFromObjectsGetter)
import Prelude (const, pure, (<>), (>=>))

identity' :: forall e. ObjectsGetter e
identity' id = pure [id]

-----------------------------------------------------------
-- SYSTEM GETTERS
-- These getters are defined on other members of PerspectRol and PerspectContext than
-- rolInContext (PerspectContext) or properties (PerspectRol). All are memorizing.
-----------------------------------------------------------

identity :: forall e. TypedTripleGetter e
identity = constructTripleGetterFromObjectsGetter "model:Perspectives$identity" identity'
  "model:Perspectives$Context"
  "model:Perspectives$Context"

contextType :: forall e. TypedTripleGetter e
contextType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getContextType
  "model:Perspectives$Context"
  "model:Perspectives$Context"

buitenRol :: forall e. TypedTripleGetter e
buitenRol = constructTripleGetterFromObjectsGetter "model:Perspectives$buitenRol" getBuitenRol
  "model:Perspectives$Context"
  "model:Perspectives$Rol"

iedereRolInContext :: forall e. TypedTripleGetter e
iedereRolInContext =  constructTripleGetterFromObjectsGetter "model:Perspectives$iedereRolInContext" getRollen
  "model:Perspectives$Context"
  "model:Perspectives$Rol"

-- | The names of every rol given to this context.
rolTypen :: forall e. TypedTripleGetter e
rolTypen =  constructTripleGetterFromObjectsGetter "model:Perspectives$rolTypen" getRolTypen
  "model:Perspectives$Rol"
  "model:Perspectives$Context"

rolType :: forall e. TypedTripleGetter e
rolType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getRolType
  "model:Perspectives$Rol"
  "model:Perspectives$Context"

binding :: forall e. TypedTripleGetter e
binding = constructTripleGetterFromObjectsGetter "model:Perspectives$binding" getRolBinding
  "model:Perspectives$Rol"
  "model:Perspectives$Rol"

rolContext :: forall e. TypedTripleGetter e
rolContext = constructTripleGetterFromObjectsGetter "model:Perspectives$context" getRolContext
  "model:Perspectives$Rol"
  "model:Perspectives$Context"

label :: forall e. TypedTripleGetter e
label = constructTripleGetterFromObjectsGetter "model:Perspectives$label" getDisplayName
  "model:Perspectives$Context"
  "model:Perspectives$String"

rolHasType :: forall e. ID -> TypedTripleGetter e
rolHasType typeId = constructTripleGetterFromObjectsGetter ("model:Perspectives$rolHasType" <> "_" <> typeId)
  (getRolType >=> \(objs::Array String) -> pure (maybe ["false"] (const ["true"]) (elemIndex typeId objs)))
  "model:Perspectives$Rol"
  -- TODO. A boolean is not really a type in perspectives, but a range.
  "model:Perspectives$Context"

-----------------------------------------------------------
-- GETTERS BASED ON MODEL:PERSPECTIVES$
-- These getters are based on properties (defined for roles) and roles (defined for contexts)
-- as modelled in the definitions of CRL and ARC.
-----------------------------------------------------------
-- | Both Roles and Properties have the property psp:isFunctional. Hence we model the
-- | domain as psp:Context.
isFunctional :: forall e. TypedTripleGetter e
isFunctional = constructExternalPropertyGetter "model:Perspectives$isFunctional"
  "model:Perspectives$Context"
  "model:Perspectives$Boolean"

isVerplicht :: forall e. TypedTripleGetter e
isVerplicht = constructExternalPropertyGetter "model:Perspectives$isVerplicht"
  "model:Perspectives$Property"
  "model:Perspectives$Boolean"

range :: forall e. TypedTripleGetter e
range = constructExternalPropertyGetter "model:Perspectives$range"
  "model:Perspectives$Rol"
  "model:Perspectives$Boolean"

hasLabel :: forall e. TypedTripleGetter e
hasLabel = QC.notEmpty label

hasBinding :: forall e. TypedTripleGetter e
hasBinding = QC.notEmpty binding

rolUser :: forall e. TypedTripleGetter e
rolUser = QC.closure' binding

lijdendVoorwerpBepaling :: forall e. TypedTripleGetter e
lijdendVoorwerpBepaling = (constructRolGetter "model:Perspectives$lijdendVoorwerpBepaling"
  "model:Perspectives$Actie") >-> binding

propertyReferentie :: forall e. TypedTripleGetter e
propertyReferentie = constructRolGetter "model:Perspectives$propertyReferentie"
  "model:Perspectives$View"

isContext :: forall e. TypedTripleGetter e
isContext = QC.notEmpty rolContext

boundContexts :: forall e. TypedTripleGetter e
boundContexts = (QC.filter (rolHasType "model:Perspectives$BuitenRol") (iedereRolInContext >-> binding)) >-> rolContext

rolPropertyTypes :: forall e. TypedTripleGetter e
rolPropertyTypes = constructRolGetter "model:Perspectives$rolProperty"
  "model:Perspectives$Rol"

contextRolTypes :: forall e. TypedTripleGetter e
contextRolTypes = constructRolGetter "model:Perspectives$rolInContext"
  "model:Perspectives$Context"

mogelijkeBinding :: forall e. TypedTripleGetter e
mogelijkeBinding = (constructRolGetter "model:Perspectives$mogelijkeBinding"
  "model:Perspectives$Rol") >-> binding

rolInContext :: forall e. TypedTripleGetter e
rolInContext = constructRolGetter "model:Perspectives$rolInContext"
  "model:Perspectives$Context"

aspect :: forall e. TypedTripleGetter e
aspect = constructRolGetter "model:Perspectives$rolInContext" "model:Perspectives$aspect" >-> binding >-> rolContext

-- | All aspects but excluding the subject itself.
aspecten :: forall e. TypedTripleGetter e
aspecten = QC.closure aspect
