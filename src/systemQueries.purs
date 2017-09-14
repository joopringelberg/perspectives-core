module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.QueryCombinators as QC
import Data.Maybe (Maybe(..))
import Perspectives.Location (nameFunction)
import Perspectives.Property (MemoizingSingleGetter, PluralGetter, SingleGetter, MemoizingPluralGetter, getResource, getResources, getString)
import Perspectives.ResourceTypes (Resource(..))
import Prelude (pure, (<<<), ($))

identifier :: SingleGetter String
identifier (Resource{id})= nameFunction "identifier" (pure <<< Just) id

label :: MemoizingSingleGetter String
label = liftSingleGetter (getString "rdfs:label")

subClassOf :: MemoizingPluralGetter Resource
subClassOf = liftPluralGetter (getResources "rdfs:subClassOf")

rdfType :: MemoizingSingleGetter Resource
rdfType = liftSingleGetter (getResource "rdf:type")

-- types :: PluralGetter Resource
-- types = nameFunction "types" (QC.mclosure rdfType)

-- superClasses :: PluralGetter Resource
-- superClasses = nameFunction "superClasses" (QC.aclosure subClassOf)

-- typeSuperClasses :: PluralGetter Resource
-- typeSuperClasses = QC.cons rdfType (rdfType >->> superClasses)
-- typeSuperClasses = (|->) rdfType >->> QC.cons QC.identity superClasses

rol_RolBinding :: MemoizingSingleGetter Resource
rol_RolBinding = liftSingleGetter (getResource "model:SysteemDomein#rol_RolBinding")
