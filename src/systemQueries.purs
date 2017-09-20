module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.QueryCombinators as QC
import Data.Maybe (Maybe(..), maybe)
import Perspectives.Location (locate, locationValue, memorizeMonadicFunction, nameFunction)
import Perspectives.Property (MemorizingPluralGetter, MemorizingSingleGetter, getResource, getResources, getString)
import Perspectives.ResourceTypes (Resource(..))
import Prelude (pure, (<<<), ($))

identifier :: MemorizingSingleGetter String
identifier = pure <<< (maybe (locate Nothing) (locate <<< Just <<< (\(Resource {id}) -> id ))) <<< locationValue

label :: MemorizingSingleGetter String
label = memorizeSingleGetter (getString "rdfs:label")

subClassOf :: MemorizingPluralGetter Resource
subClassOf = memorizePluralGetter (getResources "rdfs:subClassOf")

rdfType :: MemorizingSingleGetter Resource
rdfType = memorizeSingleGetter (getResource "rdf:type")

rol_RolBinding :: MemorizingSingleGetter Resource
rol_RolBinding = memorizeSingleGetter (getResource "model:SysteemDomein#rol_RolBinding")

-- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
types :: MemorizingPluralGetter Resource
types = memorizeMonadicFunction $ nameFunction "types" (QC.mclosure rdfType)

-- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
superClasses :: MemorizingPluralGetter Resource
superClasses = nameFunction "superClasses" (QC.aclosure subClassOf)

typeSuperClasses :: MemorizingPluralGetter Resource
-- typeSuperClasses = nameFunction "typeSuperClasses" (QC.cons rdfType (rdfType >->> superClasses))
typeSuperClasses = nameFunction "typeSuperClasses" (rdfType >->> QC.cons QC.identity superClasses)

hasLabel :: MemorizingSingleGetter Boolean
hasLabel = QC.hasValue label

hasBinding :: MemorizingSingleGetter Boolean
hasBinding = QC.hasValue rol_RolBinding
