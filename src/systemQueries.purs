module Perspectives.SystemQueries where

import Data.Array (elemIndex)
import Data.Maybe (maybe)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Property (ObjectsGetter, getBuitenRol, getContextType, getDisplayName, getRolBinding, getRolContext, getRolType, getRolTypen, getRollen)
import Perspectives.PropertyComposition ((>->))
import Perspectives.QueryCombinators (hasValue, closure', filter) as QC
import Perspectives.TripleGetter (NamedTripleGetter, constructPublicPropertyGetter, constructRolGetter, constructTripleGetterFromArbitraryFunction)
import Prelude (const, pure, (<>), (>=>))

identity' :: forall e. ObjectsGetter e
identity' id = pure [id]

-----------------------------------------------------------
-- SYSTEM GETTERS
-- These getters are defined on other members of PerspectRol and PerspectContext than
-- rolInContext (PerspectContext) or properties (PerspectRol). All are memorizing.
-----------------------------------------------------------

identity :: forall e. NamedTripleGetter e
identity = constructTripleGetterFromArbitraryFunction "model:Perspectives$identity" identity'

contextType :: forall e. NamedTripleGetter e
contextType = constructTripleGetterFromArbitraryFunction "model:Perspectives$type" getContextType

buitenRol :: forall e. NamedTripleGetter e
buitenRol = constructTripleGetterFromArbitraryFunction "model:Perspectives$buitenRol" getBuitenRol

iedereRolInContext :: forall e. NamedTripleGetter e
iedereRolInContext =  constructTripleGetterFromArbitraryFunction "model:Perspectives$iedereRolInContext" getRollen

rolTypen :: forall e. NamedTripleGetter e
rolTypen =  constructTripleGetterFromArbitraryFunction "model:Perspectives$rolTypen" getRolTypen

rolType :: forall e. NamedTripleGetter e
rolType = constructTripleGetterFromArbitraryFunction "model:Perspectives$type" getRolType

binding :: forall e. NamedTripleGetter e
binding = constructTripleGetterFromArbitraryFunction "model:Perspectives$binding" getRolBinding

rolContext :: forall e. NamedTripleGetter e
rolContext = constructTripleGetterFromArbitraryFunction "model:Perspectives$context" getRolContext

label :: forall e. NamedTripleGetter e
label = constructTripleGetterFromArbitraryFunction "model:Perspectives$label" getDisplayName

rolHasType :: forall e. ID -> NamedTripleGetter e
rolHasType typeId = constructTripleGetterFromArbitraryFunction ("model:Perspectives$rolHasType" <> "_" <> typeId)
  (getRolType >=> \(objs::Array String) -> pure (maybe ["false"] (const ["true"]) (elemIndex typeId objs)))

-----------------------------------------------------------
-- GETTERS BASED ON MODEL:PERSPECTIVES$
-- These getters are based on properties (defined for roles) and roles (defined for contexts)
-- as modelled in the definitions of CRL and ARC.
-----------------------------------------------------------
isFunctional :: forall e. NamedTripleGetter e
isFunctional = constructPublicPropertyGetter "model:Perspectives$isFunctional"

isVerplicht :: forall e. NamedTripleGetter e
isVerplicht = constructPublicPropertyGetter "model:Perspectives$isVerplicht"

range :: forall e. NamedTripleGetter e
range = constructPublicPropertyGetter "model:Perspectives$range"

hasLabel :: forall e. NamedTripleGetter e
hasLabel = QC.hasValue label

hasBinding :: forall e. NamedTripleGetter e
hasBinding = QC.hasValue binding

rolUser :: forall e. NamedTripleGetter e
rolUser = QC.closure' binding

lijdendVoorwerpBepaling :: forall e. NamedTripleGetter e
lijdendVoorwerpBepaling = constructRolGetter "model:Perspectives$lijdendVoorwerpBepaling"

propertyReferentie :: forall e. NamedTripleGetter e
propertyReferentie = constructRolGetter "model:Perspectives$propertyReferentie"

isContext :: forall e. NamedTripleGetter e
isContext = QC.hasValue rolContext

boundContexts :: forall e. NamedTripleGetter e
boundContexts = (QC.filter (rolHasType "model:Perspectives$BuitenRol") (iedereRolInContext >-> binding)) >-> rolContext
