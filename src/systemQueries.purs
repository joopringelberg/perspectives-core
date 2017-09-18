module Perspectives.SystemQueries where

import Perspectives.PropertyComposition
import Perspectives.QueryCombinators as QC
import Data.Maybe (Maybe(..))
import Perspectives.Location (locate, memoizeMonadicFunction, nameFunction)
import Perspectives.Property (MemoizingPluralGetter, MemoizingSingleGetter, getResource, getResources, getString)
import Perspectives.Resource (resourceFromLocation)
import Perspectives.ResourceTypes (Resource(..))
import Prelude (pure, (<<<), ($))

identifier :: MemoizingSingleGetter String
identifier = memoizeMonadicFunction $ nameFunction "identifier"
  (pure <<< locate <<< Just <<< (\(Resource {id}) -> id ) <<< resourceFromLocation)

label :: MemoizingSingleGetter String
label = liftSingleGetter (getString "rdfs:label")

subClassOf :: MemoizingPluralGetter Resource
subClassOf = liftPluralGetter (getResources "rdfs:subClassOf")

rdfType :: MemoizingSingleGetter Resource
rdfType = liftSingleGetter (getResource "rdf:type")

rol_RolBinding :: MemoizingSingleGetter Resource
rol_RolBinding = liftSingleGetter (getResource "model:SysteemDomein#rol_RolBinding")

-- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
types :: MemoizingPluralGetter Resource
types = memoizeMonadicFunction $ nameFunction "types" (QC.mclosure rdfType)

-- | NB. Dit is onvoldoende. Alleen de 'buitenste' aanroep wordt gememoiseerd; niet de recursieve.
superClasses :: MemoizingPluralGetter Resource
superClasses = QC.aclosure subClassOf

typeSuperClasses :: MemoizingPluralGetter Resource
typeSuperClasses = nameFunction "typeSuperClasses" (QC.cons rdfType (rdfType >->> superClasses))
-- typeSuperClasses = (|->) rdfType >->> QC.cons QC.identity superClasses

hasLabel :: MemoizingSingleGetter Boolean
hasLabel = nameFunction "hasLabel" (QC.hasValue label)
