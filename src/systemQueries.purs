module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.Property (Getter)
import Perspectives.TripleAdministration (NamedTripleGetter, constructTripleGetter, constructTripleGetterFromArbitraryFunction)
import Prelude (pure)

identifier' :: Getter
identifier' id = pure [id]

identifier :: NamedTripleGetter
identifier = constructTripleGetterFromArbitraryFunction "identifier" identifier'

label :: NamedTripleGetter
label = constructTripleGetter "rdfs:label"

subClassOf :: NamedTripleGetter
subClassOf = constructTripleGetter "rdfs:subClassOf"

rdfType :: NamedTripleGetter
rdfType = constructTripleGetter "rdf:type"

rol_RolBinding :: NamedTripleGetter
rol_RolBinding = constructTripleGetter "model:SysteemDomein#rol_RolBinding"

-- -- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
-- types :: StackedMemorizingPluralGetter Resource
-- types = (QC.mclosure rdfType "types")
--
-- -- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
-- superClasses :: StackedMemorizingPluralGetter Resource
-- superClasses = nameFunction "superClasses" (QC.aclosure subClassOf "superClasses")
--
-- typeSuperClasses :: StackedMemorizingPluralGetter Resource
-- typeSuperClasses = nameFunction "typeSuperClasses" (QC.addTo rdfType (rdfType >->> superClasses))
-- -- -- typeSuperClasses = nameFunction "typeSuperClasses" (rdfType >->> QC.addTo QC.identity superClasses)
--
-- hasLabel :: StackedMemorizingSingleGetter Boolean
-- hasLabel = QC.hasValue label
--
-- hasBinding :: StackedMemorizingSingleGetter Boolean
-- hasBinding = QC.hasValue rol_RolBinding

isFunctional :: NamedTripleGetter
isFunctional = constructTripleGetter "owl:FunctionalProperty"

rdfsRange :: NamedTripleGetter
rdfsRange = constructTripleGetter "rdfs:range"

owlInverseOf :: NamedTripleGetter
owlInverseOf = constructTripleGetter "owl:inverseOf"
