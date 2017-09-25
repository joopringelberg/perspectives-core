module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.QueryCombinators as QC
import Data.Maybe (Maybe(..))
import Perspectives.Location (nameFunction, nestLocationInMonad)
import Perspectives.Property (MemorizingPluralGetter, MemorizingSingleGetter, SingleGetter, getResource, getResources, getString)
import Perspectives.ResourceTypes (Resource(..))
import Prelude (pure)

identifier' :: SingleGetter String
identifier' Nothing = pure Nothing
identifier' (Just (Resource {id})) = pure (Just id)

identifier :: MemorizingSingleGetter String
identifier = nestLocationInMonad (nameFunction "identifier" identifier')

label :: MemorizingSingleGetter String
label = nestLocationInMonad (getString "rdfs:label")

subClassOf :: MemorizingPluralGetter Resource
subClassOf = nestLocationInMonad (getResources "rdfs:subClassOf")

rdfType :: MemorizingSingleGetter Resource
rdfType = nestLocationInMonad (getResource "rdf:type")

rol_RolBinding :: MemorizingSingleGetter Resource
rol_RolBinding = nestLocationInMonad (getResource "model:SysteemDomein#rol_RolBinding")

-- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
types :: MemorizingPluralGetter Resource
types = nameFunction "types" (QC.mclosure rdfType)

-- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
superClasses :: MemorizingPluralGetter Resource
superClasses = nameFunction "superClasses" (QC.aclosure subClassOf)

typeSuperClasses :: MemorizingPluralGetter Resource
typeSuperClasses = nameFunction "typeSuperClasses" (QC.addTo rdfType (rdfType >->> superClasses))
-- -- typeSuperClasses = nameFunction "typeSuperClasses" (rdfType >->> QC.addTo QC.identity superClasses)

hasLabel :: MemorizingSingleGetter Boolean
hasLabel = QC.hasValue label

hasBinding :: MemorizingSingleGetter Boolean
hasBinding = QC.hasValue rol_RolBinding
