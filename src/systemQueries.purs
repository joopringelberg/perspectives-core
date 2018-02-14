module Perspectives.SystemQueries where

import Perspectives.Property (ObjectsGetter, getBuitenRol, getContextType, getDisplayName, getRolBinding, getRolContext, getRolType, getRolTypen, getRollen)
import Perspectives.QueryCombinators (hasValue, closure') as QC
import Perspectives.TripleGetter (NamedTripleGetter, constructPublicPropertyGetter, constructTripleGetterFromArbitraryFunction)
import Prelude (pure)

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
