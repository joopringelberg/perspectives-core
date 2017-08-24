module Perspectives.SystemQueries where

import Prelude (($), pure)
import Data.Maybe(Maybe(..))
import Perspectives.Property (SingleGetter, PluralGetter, getResource, getResources, getString)
import Perspectives.ResourceTypes (Resource(..))
import Perspectives.QueryCombinators as QC
import Perspectives.PropertyComposition

identifier :: SingleGetter String
identifier (Resource{id})= pure $ Just id

rol_RolBinding :: SingleGetter Resource
rol_RolBinding = getResource "model:SysteemDomein#rol_RolBinding"

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
