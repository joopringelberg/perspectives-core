module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.QueryCombinators as QC
import Data.Maybe (Maybe(..))
import Perspectives.Location (nameFunction)
import Perspectives.Property (SingleGetter, StackedMemorizingSingleGetter, StackedMemorizingPluralGetter, getResource, getResources, getString)
import Perspectives.ResourceTypes (Resource(..))
import Prelude (pure)

identifier' :: SingleGetter String
identifier' Nothing = pure Nothing
identifier' (Just (Resource {id})) = pure (Just id)

identifier :: StackedMemorizingSingleGetter String
identifier = memorizeInStackedLocation (nameFunction "identifier" identifier')

label :: StackedMemorizingSingleGetter String
label = memorizeInStackedLocation (getString "rdfs:label")

subClassOf :: StackedMemorizingPluralGetter Resource
subClassOf = memorizeInStackedLocation (getResources "rdfs:subClassOf")

rdfType :: StackedMemorizingSingleGetter Resource
rdfType = memorizeInStackedLocation (getResource "rdf:type")

rol_RolBinding :: StackedMemorizingSingleGetter Resource
rol_RolBinding = memorizeInStackedLocation (getResource "model:SysteemDomein#rol_RolBinding")

-- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
types :: StackedMemorizingPluralGetter Resource
types = nameFunction "types" (QC.mclosure rdfType)

-- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
superClasses :: StackedMemorizingPluralGetter Resource
superClasses = nameFunction "superClasses" (QC.aclosure subClassOf)

typeSuperClasses :: StackedMemorizingPluralGetter Resource
typeSuperClasses = nameFunction "typeSuperClasses" (QC.addTo rdfType (rdfType >->> superClasses))
-- -- typeSuperClasses = nameFunction "typeSuperClasses" (rdfType >->> QC.addTo QC.identity superClasses)

hasLabel :: StackedMemorizingSingleGetter Boolean
hasLabel = QC.hasValue label

hasBinding :: StackedMemorizingSingleGetter Boolean
hasBinding = QC.hasValue rol_RolBinding
