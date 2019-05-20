module Perspectives.StringTripleGetterConstructors
( module Perspectives.StringTripleGetterConstructors,
  module TGCreExports
)
where

import Control.Alt ((<|>))
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (type (**>), TripleGetter, TypedTripleGetter(..), (@@), MPQ, Triple)
import Perspectives.DataTypeTripleGetters (binnenRol, buitenRol, genericBinding, genericContext, binding, context) as DTG
import Perspectives.Identifiers (LocalName, hasLocalName) as Id
import Perspectives.ObjectGetterConstructors (directAspectProperties, directAspectRoles, getContextRol, getUnqualifiedContextRol, genericGetRoleBinders) as OGC
import Perspectives.PerspectivesTypes (RolDef(..), genericGetProperty, genericGetUnqualifiedLocalProperty, typeWithPerspectivesTypes)
import Perspectives.QueryCombinators (filter_) as QC
import Perspectives.TripleGetterComposition (before, (>->), preferLeft)
import Perspectives.TripleGetterConstructors (closure, searchInRolTelescope, unlessNull, directAspects, searchRolInContext, directAspectRoles, getInternalProperty) as TGC
import Perspectives.TripleGetterConstructors (closure, unlessNull, searchInRolTelescope, directAspects, concat, some, all, closureOfAspect, getPrototype, closureOfPrototype) as TGCreExports
import Perspectives.TripleGetterFromObjectGetter (trackedAs)
import Prelude (flip, (<>), ($))
-----------------------------------------------------------
-- COMBINATORS
-----------------------------------------------------------
type StringTripleGetter = TripleGetter String String
type StringTypedTripleGetter = (String **> String)

searchInPrototypeHierarchy :: StringTypedTripleGetter -> StringTypedTripleGetter
searchInPrototypeHierarchy getter = typeWithPerspectivesTypes DTG.buitenRol >-> TGC.searchInRolTelescope getter

searchLocallyAndInPrototypeHierarchy :: StringTypedTripleGetter -> StringTypedTripleGetter
searchLocallyAndInPrototypeHierarchy getter = getter `preferLeft` (\_ -> (DTG.buitenRol >-> DTG.binding >-> DTG.context >-> searchLocallyAndInPrototypeHierarchy getter)) $ "searchLocallyAndInPrototypeHierarchy"

searchInAspectsAndPrototypes :: StringTypedTripleGetter -> StringTypedTripleGetter
searchInAspectsAndPrototypes getter@(TypedTripleGetter n _) = TypedTripleGetter n f where
  f :: StringTripleGetter
  f contextId =
    TGC.unlessNull (searchLocallyAndInPrototypeHierarchy getter) contextId
    <|>
    (contextId @@ (TGC.directAspects >-> searchInAspectsAndPrototypes getter))

-- | Applies the StringTypedTripleGetter to the RolDef and all its prototypes and recursively to all its aspects.
-- Test.Perspectives.ModelBasedTripleGetters, via propertiesDef
searchInAspectRolesAndPrototypes :: StringTypedTripleGetter -> StringTypedTripleGetter
searchInAspectRolesAndPrototypes getter@(TypedTripleGetter n _) = TypedTripleGetter n f where
  f :: StringTripleGetter
  f rolDefId =
    TGC.unlessNull (searchLocallyAndInPrototypeHierarchy getter) rolDefId
    <|>
    (rolDefId @@ (RolDef `before` TGC.directAspectRoles >-> unwrap `before` searchInAspectRolesAndPrototypes getter))

directAspectRoles :: StringTypedTripleGetter
directAspectRoles = typeWithPerspectivesTypes OGC.directAspectRoles `trackedAs` "model:Perspectives$Rol$directAspectRoles"

directAspectProperties :: StringTypedTripleGetter
directAspectProperties = typeWithPerspectivesTypes OGC.directAspectProperties `trackedAs` "model:Perspectives$Property$directAspectProperties"

-----------------------------------------------------------
-- CLOSURES
-----------------------------------------------------------
closureOfBinding :: StringTypedTripleGetter
closureOfBinding = TGC.closure DTG.genericBinding

closureOfAspectRol :: StringTypedTripleGetter
closureOfAspectRol = TGC.closure directAspectRoles

closureOfAspectProperty :: StringTypedTripleGetter
closureOfAspectProperty = TGC.closure directAspectProperties

-----------------------------------------------------------
-- GET A ROL FROM A CONTEXT
-----------------------------------------------------------
getContextRol :: String -> StringTypedTripleGetter
getContextRol rn = typeWithPerspectivesTypes OGC.getContextRol rn `trackedAs` rn

getRolInContext :: String -> StringTypedTripleGetter
getRolInContext = getContextRol

getUnqualifiedContextRol :: Id.LocalName -> StringTypedTripleGetter
getUnqualifiedContextRol ln = typeWithPerspectivesTypes OGC.getUnqualifiedContextRol ln `trackedAs` ln

getUnqualifiedRolInContext :: Id.LocalName -> StringTypedTripleGetter
getUnqualifiedRolInContext = getUnqualifiedContextRol

rolesOf :: String -> StringTypedTripleGetter
rolesOf cid = TypedTripleGetter ("rolesOf_" <> cid) (typeWithPerspectivesTypes getter) where
  getter :: String -> MPQ (Triple String String)
  getter rd = cid @@ typeWithPerspectivesTypes TGC.searchRolInContext rd

-----------------------------------------------------------
-- SEARCH A ROL IN A CONTEXT AND ITS PROTOTYPES
-----------------------------------------------------------
-- | Search for a qualified ContextRol both in the local context and all its prototypes.
searchContextRol :: String -> StringTypedTripleGetter
searchContextRol rn = searchLocallyAndInPrototypeHierarchy ((getContextRol rn))

searchRolInContext :: String -> StringTypedTripleGetter
searchRolInContext = searchContextRol

-- TODO. Waarom eerst de context nemen?
searchUnqualifiedRol :: Id.LocalName -> StringTypedTripleGetter
searchUnqualifiedRol rn = searchLocallyAndInPrototypeHierarchy (getUnqualifiedContextRol rn)

-----------------------------------------------------------
-- GET A ROLDEFINITION FROM A CONTEXT DEFINITION
-----------------------------------------------------------
getUnqualifiedRolDefinition :: Id.LocalName -> StringTypedTripleGetter
getUnqualifiedRolDefinition ln = (QC.filter_
  (flip Id.hasLocalName ln)
  ("hasLocalName_" <> ln)
  (getUnqualifiedContextRol "rolInContext" >-> DTG.genericBinding >-> DTG.genericContext))

searchUnqualifiedRolDefinition :: Id.LocalName -> StringTypedTripleGetter
searchUnqualifiedRolDefinition ln = searchInAspectsAndPrototypes (getUnqualifiedRolDefinition ln)

-----------------------------------------------------------
-- GET A PROPERTY FROM A ROLE TELESCOPE
-----------------------------------------------------------
searchProperty :: String -> StringTypedTripleGetter
searchProperty pd = TGC.searchInRolTelescope g
  where
    g :: (String **> String)
    g = genericGetProperty pd `trackedAs` pd

searchUnqualifiedProperty :: Id.LocalName -> StringTypedTripleGetter
searchUnqualifiedProperty pd = TGC.searchInRolTelescope g
  where
    g :: StringTypedTripleGetter
    g = genericGetUnqualifiedLocalProperty pd `trackedAs` pd

-----------------------------------------------------------
-- GET A PROPERTY FROM A CONTEXT
-----------------------------------------------------------
searchPropertyOnContext :: StringTypedTripleGetter -> String -> StringTypedTripleGetter
searchPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: StringTypedTripleGetter
    f = DTG.genericContext >-> rolgetter >-> searchProperty p

searchUnqualifiedPropertyOnContext :: StringTypedTripleGetter  -> Id.LocalName -> StringTypedTripleGetter
searchUnqualifiedPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: StringTypedTripleGetter
    f = DTG.genericContext >-> rolgetter >-> searchUnqualifiedProperty p

searchExternalProperty :: String -> StringTypedTripleGetter
searchExternalProperty pn = searchPropertyOnContext (typeWithPerspectivesTypes DTG.buitenRol) pn

searchExternalUnqualifiedProperty :: Id.LocalName -> StringTypedTripleGetter
searchExternalUnqualifiedProperty ln = searchUnqualifiedPropertyOnContext (typeWithPerspectivesTypes DTG.buitenRol) ln

getInternalProperty :: String -> StringTypedTripleGetter
getInternalProperty = typeWithPerspectivesTypes TGC.getInternalProperty

searchInternalUnqualifiedProperty :: Id.LocalName -> StringTypedTripleGetter
searchInternalUnqualifiedProperty ln = typeWithPerspectivesTypes DTG.binnenRol >-> searchUnqualifiedProperty ln

getRoleBinders :: String -> StringTypedTripleGetter
getRoleBinders rname = OGC.genericGetRoleBinders rname `trackedAs` rname

-----------------------------------------------------------
-- GET A PROPERTYDEFINITION FROM A ROL DEFINITION
-----------------------------------------------------------
getUnqualifiedPropertyDefinition :: Id.LocalName -> StringTypedTripleGetter
getUnqualifiedPropertyDefinition ln = (QC.filter_
  (flip Id.hasLocalName ln)
  ("hasLocalName_" <> ln)
  (getUnqualifiedContextRol "rolInContext" >-> DTG.genericBinding >-> DTG.genericContext))

searchUnqualifiedPropertyDefinition :: Id.LocalName -> StringTypedTripleGetter
searchUnqualifiedPropertyDefinition ln = searchInAspectsAndPrototypes (getUnqualifiedPropertyDefinition ln)

-----------------------------------------------------------
-- INVERSE ROL
-----------------------------------------------------------
constructInverseRolGetter ::
  String ->
  StringTypedTripleGetter
constructInverseRolGetter pn = (OGC.genericGetRoleBinders pn) `trackedAs` (pn <> "_inverse")

-- | The PropertyReferences of the View. Again, the typing is imprecise.
propertyReferenties :: StringTypedTripleGetter
propertyReferenties = typeWithPerspectivesTypes searchUnqualifiedRol "propertyReferentie"
