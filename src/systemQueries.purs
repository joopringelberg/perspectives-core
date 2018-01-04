module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.Property (ObjectsGetter, getBuitenRol, getContextType, getRolBinding, getRolContext, getRolType, getRolTypen, getRollen)
import Perspectives.QueryCombinators (closure, concat, hasValue) as QC
import Perspectives.TripleGetter (NamedTripleGetter, constructPublicPropertyGetter, constructTripleGetter, constructTripleGetterFromArbitraryFunction)
import Prelude (pure)

identity' :: forall e. ObjectsGetter e
identity' id = pure [id]

identity :: forall e. NamedTripleGetter e
identity = constructTripleGetterFromArbitraryFunction "identity" identity'

label :: forall e. NamedTripleGetter e
label = constructTripleGetter "rdfs:label"

subClassOf :: forall e. NamedTripleGetter e
subClassOf = constructTripleGetter "rdfs:subClassOf"

rdfType :: forall e. NamedTripleGetter e
rdfType = constructTripleGetter "rdf:type"

rol_RolBinding :: forall e. NamedTripleGetter e
rol_RolBinding = constructTripleGetter "model:SysteemDomein$rol_RolBinding"

types :: forall e. NamedTripleGetter e
types = QC.closure rdfType

superClasses :: forall e. NamedTripleGetter e
superClasses = QC.closure subClassOf

typeSuperClasses :: forall e. NamedTripleGetter e
typeSuperClasses = QC.concat rdfType (rdfType >-> superClasses)
-- typeSuperClasses = rdfType >->> QC.concat identity superClasses

hasLabel :: forall e. NamedTripleGetter e
hasLabel = QC.hasValue label

hasBinding :: forall e. NamedTripleGetter e
hasBinding = QC.hasValue rol_RolBinding

-- isFunctional :: forall e. NamedTripleGetter e
-- isFunctional = constructTripleGetter "owl:FunctionalProperty"

rdfsRange :: forall e. NamedTripleGetter e
rdfsRange = constructTripleGetter "rdfs:range"

owlInverseOf :: forall e. NamedTripleGetter e
owlInverseOf = constructTripleGetter "owl:inverseOf"

-----------------------------------------------------------
-- Context and Role TripleGetters.
-----------------------------------------------------------

contextType :: forall e. NamedTripleGetter e
contextType = constructTripleGetterFromArbitraryFunction "psp:type" getContextType

buitenRol :: forall e. NamedTripleGetter e
buitenRol = constructTripleGetterFromArbitraryFunction "psp:buitenRol" getBuitenRol

rollen :: forall e. NamedTripleGetter e
rollen =  constructTripleGetterFromArbitraryFunction "psp:rollen" getRollen

rolTypen :: forall e. NamedTripleGetter e
rolTypen =  constructTripleGetterFromArbitraryFunction "psp:rolTypen" getRolTypen

rolType :: forall e. NamedTripleGetter e
rolType = constructTripleGetterFromArbitraryFunction "psp:type" getRolType

binding :: forall e. NamedTripleGetter e
binding = constructTripleGetterFromArbitraryFunction "psp:binding" getRolBinding

rolContext :: forall e. NamedTripleGetter e
rolContext = constructTripleGetterFromArbitraryFunction "psp:context" getRolContext

isFunctional :: forall e. NamedTripleGetter e
isFunctional = constructPublicPropertyGetter "psp:isFunctional"

isVerplicht :: forall e. NamedTripleGetter e
isVerplicht = constructPublicPropertyGetter "psp:isVerplicht"

range :: forall e. NamedTripleGetter e
range = constructPublicPropertyGetter "psp:range"
