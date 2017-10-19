module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.QueryCombinators as QC
import Data.Maybe (Maybe(..))
import Perspectives.Location (nameFunction)
import Perspectives.Property (SingleGetter, StackedMemorizingPluralGetter, StackedMemorizingSingleGetter, getBoolean)
import Perspectives.ResourceTypes (Resource(..))
import Perspectives.Triples (NamedSingleTripleGetter, NamedPluralTripleGetter, getResources, getString, getResource)
import Prelude (pure)

identifier' :: SingleGetter String
identifier' Nothing = pure Nothing
identifier' (Just (Resource {id})) = pure (Just id)

identifier :: StackedMemorizingSingleGetter String
identifier = memorizeInStackedLocation (nameFunction "identifier" identifier')

label :: NamedSingleTripleGetter String
label = getString "rdfs:label"

subClassOf :: NamedPluralTripleGetter Resource
subClassOf = getResources "rdfs:subClassOf"

rdfType :: NamedSingleTripleGetter Resource
rdfType = getResource "rdf:type"

rol_RolBinding :: NamedSingleTripleGetter Resource
rol_RolBinding = getResource "model:SysteemDomein#rol_RolBinding"

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

isFunctional :: StackedMemorizingSingleGetter Boolean
isFunctional = memorizeInStackedLocation (getBoolean "owl:FunctionalProperty")

rdfsRange :: NamedSingleTripleGetter String
rdfsRange = getString "rdfs:range"

owlInverseOf :: NamedSingleTripleGetter Resource
owlInverseOf = getResource "owl:inverseOf"
