module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.QueryCombinators as QC
import Data.Maybe (Maybe(..))
import Perspectives.Property (PluralGetter, SingleGetter, getResource, getResources, getString)
import Perspectives.ResourceTypes (Resource(..))
import Prelude (($), pure)

identifier :: SingleGetter String
identifier (Resource{id})= pure $ Just id

label :: SingleGetter String
label = getString "rdfs:label"

subClassOf :: PluralGetter Resource
subClassOf = getResources "rdfs:subClassOf"

rdfType :: SingleGetter Resource
rdfType = getResource "rdf:type"

types :: PluralGetter Resource
types = QC.mclosure rdfType

superClasses :: PluralGetter Resource
superClasses = QC.aclosure subClassOf

typeSuperClasses :: PluralGetter Resource
typeSuperClasses = QC.cons rdfType (rdfType >->> superClasses)
-- typeSuperClasses = rdfType >->> QC.cons QC.identity superClasses

rol_RolBinding :: SingleGetter Resource
rol_RolBinding = getResource "model:SysteemDomein#rol_RolBinding"

test = query rol_RolBinding >-> rdfType
