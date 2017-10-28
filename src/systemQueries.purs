module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.Property (ObjectsGetter)
import Perspectives.QueryCombinators (closure, concat, hasValue) as QC
import Perspectives.TripleGetter (NamedTripleGetter, constructTripleGetter, constructTripleGetterFromArbitraryFunction)
import Prelude (pure)

identity' :: forall e. ObjectsGetter e
identity' id = pure [id]

identity :: NamedTripleGetter
identity = constructTripleGetterFromArbitraryFunction "identity" identity'

label :: NamedTripleGetter
label = constructTripleGetter "rdfs:label"

subClassOf :: NamedTripleGetter
subClassOf = constructTripleGetter "rdfs:subClassOf"

rdfType :: NamedTripleGetter
rdfType = constructTripleGetter "rdf:type"

rol_RolBinding :: NamedTripleGetter
rol_RolBinding = constructTripleGetter "model:SysteemDomein#rol_RolBinding"

types :: NamedTripleGetter
types = QC.closure rdfType

superClasses :: NamedTripleGetter
superClasses = QC.closure subClassOf

typeSuperClasses :: NamedTripleGetter
typeSuperClasses = QC.concat rdfType (rdfType >-> superClasses)
-- typeSuperClasses = rdfType >->> QC.concat identity superClasses

hasLabel :: NamedTripleGetter
hasLabel = QC.hasValue label

hasBinding :: NamedTripleGetter
hasBinding = QC.hasValue rol_RolBinding

isFunctional :: NamedTripleGetter
isFunctional = constructTripleGetter "owl:FunctionalProperty"

rdfsRange :: NamedTripleGetter
rdfsRange = constructTripleGetter "rdfs:range"

owlInverseOf :: NamedTripleGetter
owlInverseOf = constructTripleGetter "owl:inverseOf"
