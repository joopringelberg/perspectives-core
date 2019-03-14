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
import Perspectives.ObjectGetterConstructors (directAspectProperties, directAspectRoles, getContextRol, getUnqualifiedContextRol, genericGetGebondenAls) as OGC
import Perspectives.PerspectivesTypes (RolDef(..), genericGetProperty, genericGetUnqualifiedLocalProperty, typeWithPerspectivesTypes)
import Perspectives.QueryCombinators (filter_)
import Perspectives.TripleGetterComposition (before, (>->))
import Perspectives.TripleGetterConstructors (closure, searchInRolTelescope, unlessNull, directAspects, searchRolInContext, directAspectRoles) as TGC
import Perspectives.TripleGetterConstructors (closure, unlessNull, searchInRolTelescope, directAspects, concat, some, all, closureOfAspect, getPrototype, closureOfPrototype) as TGCreExports
import Perspectives.TripleGetterFromObjectGetter (trackedAs)
import Prelude (flip, (<>))
-----------------------------------------------------------
-- COMBINATORS
-----------------------------------------------------------
type StringTripleGetter e = TripleGetter String String e
type StringTypedTripleGetter e = (String **> String) e

searchInPrototypeHierarchy :: forall e. StringTypedTripleGetter e -> StringTypedTripleGetter e
searchInPrototypeHierarchy getter = typeWithPerspectivesTypes DTG.buitenRol >-> TGC.searchInRolTelescope getter

searchLocallyAndInPrototypeHierarchy :: forall e. StringTypedTripleGetter e -> StringTypedTripleGetter e
searchLocallyAndInPrototypeHierarchy getter@(TypedTripleGetter n _) = TypedTripleGetter n f where
  f :: StringTripleGetter e
  f (cid :: String) =
    TGC.unlessNull getter cid
    <|>
    (cid @@ (DTG.buitenRol >-> DTG.binding >-> DTG.context >-> searchLocallyAndInPrototypeHierarchy getter))

searchInAspectsAndPrototypes :: forall e. StringTypedTripleGetter e -> StringTypedTripleGetter e
searchInAspectsAndPrototypes getter@(TypedTripleGetter n _) = TypedTripleGetter n f where
  f :: StringTripleGetter e
  f contextId =
    TGC.unlessNull (searchLocallyAndInPrototypeHierarchy getter) contextId
    <|>
    (contextId @@ (TGC.directAspects >-> searchInAspectsAndPrototypes getter))

-- | Applies the StringTypedTripleGetter e to the RolDef and all its prototypes and recursively to all its aspects.
-- Test.Perspectives.ModelBasedTripleGetters, via propertiesDef
searchInAspectRolesAndPrototypes :: forall e. StringTypedTripleGetter e -> StringTypedTripleGetter e
searchInAspectRolesAndPrototypes getter@(TypedTripleGetter n _) = TypedTripleGetter n f where
  f :: StringTripleGetter e
  f rolDefId =
    TGC.unlessNull (searchLocallyAndInPrototypeHierarchy getter) rolDefId
    <|>
    (rolDefId @@ (RolDef `before` TGC.directAspectRoles >-> unwrap `before` searchInAspectRolesAndPrototypes getter))

directAspectRoles :: forall e. StringTypedTripleGetter e
directAspectRoles = typeWithPerspectivesTypes OGC.directAspectRoles `trackedAs` "model:Perspectives$Rol$directAspectRoles"

directAspectProperties :: forall e. StringTypedTripleGetter e
directAspectProperties = typeWithPerspectivesTypes OGC.directAspectProperties `trackedAs` "model:Perspectives$Property$directAspectProperties"

-----------------------------------------------------------
-- CLOSURES
-----------------------------------------------------------
closureOfBinding :: forall e. StringTypedTripleGetter e
closureOfBinding = TGC.closure DTG.genericBinding

closureOfAspectRol :: forall e. StringTypedTripleGetter e
closureOfAspectRol = TGC.closure directAspectRoles

closureOfAspectProperty :: forall e. StringTypedTripleGetter e
closureOfAspectProperty = TGC.closure directAspectProperties

-----------------------------------------------------------
-- GET A ROL FROM A CONTEXT
-----------------------------------------------------------
getContextRol :: forall e. String -> StringTypedTripleGetter e
getContextRol rn = typeWithPerspectivesTypes OGC.getContextRol rn `trackedAs` rn

getRolInContext :: forall e. String -> StringTypedTripleGetter e
getRolInContext = getContextRol

getUnqualifiedContextRol :: forall e. Id.LocalName -> StringTypedTripleGetter e
getUnqualifiedContextRol ln = typeWithPerspectivesTypes OGC.getUnqualifiedContextRol ln `trackedAs` ln

getUnqualifiedRolInContext :: forall e. Id.LocalName -> StringTypedTripleGetter e
getUnqualifiedRolInContext = getUnqualifiedContextRol

rolesOf :: forall e. String -> StringTypedTripleGetter e
rolesOf cid = TypedTripleGetter ("rolesOf_" <> cid) (typeWithPerspectivesTypes getter) where
  getter :: String -> MPQ e (Triple String String e)
  getter rd = cid @@ typeWithPerspectivesTypes TGC.searchRolInContext rd

-----------------------------------------------------------
-- SEARCH A ROL IN A CONTEXT AND ITS PROTOTYPES
-----------------------------------------------------------
-- | Search for a qualified ContextRol both in the local context and all its prototypes.
searchContextRol :: forall e. String -> StringTypedTripleGetter e
searchContextRol rn = searchLocallyAndInPrototypeHierarchy (DTG.genericContext >-> (getContextRol rn))

searchRolInContext :: forall e. String -> StringTypedTripleGetter e
searchRolInContext = searchContextRol

searchUnqualifiedRol :: forall e. Id.LocalName -> StringTypedTripleGetter e
searchUnqualifiedRol rn = searchLocallyAndInPrototypeHierarchy (DTG.genericContext >-> (getUnqualifiedContextRol rn))

-----------------------------------------------------------
-- GET A ROLDEFINITION FROM A CONTEXT DEFINITION
-----------------------------------------------------------
getUnqualifiedRolDefinition ::	forall e. Id.LocalName -> StringTypedTripleGetter e
getUnqualifiedRolDefinition ln = (filter_
  (flip Id.hasLocalName ln)
  ("hasLocalName_" <> ln)
  (getUnqualifiedContextRol "rolInContext" >-> DTG.genericBinding >-> DTG.genericContext))

searchUnqualifiedRolDefinition ::	forall e. Id.LocalName -> StringTypedTripleGetter e
searchUnqualifiedRolDefinition ln = searchInAspectsAndPrototypes f
  where
    f :: (String **> String) e
    f = DTG.genericContext >-> getUnqualifiedRolDefinition ln

-----------------------------------------------------------
-- GET A PROPERTY FROM A ROLE TELESCOPE
-----------------------------------------------------------
searchProperty :: forall e. String -> StringTypedTripleGetter e
searchProperty pd = TGC.searchInRolTelescope g
  where
    g :: (String **> String) e
    g = genericGetProperty pd `trackedAs` pd

searchUnqualifiedProperty :: forall e. Id.LocalName -> StringTypedTripleGetter e
searchUnqualifiedProperty pd = TGC.searchInRolTelescope g
  where
    g :: StringTypedTripleGetter e
    g = genericGetUnqualifiedLocalProperty pd `trackedAs` pd

-----------------------------------------------------------
-- GET A PROPERTY FROM A CONTEXT
-----------------------------------------------------------
searchPropertyOnContext :: forall e. StringTypedTripleGetter e -> String -> StringTypedTripleGetter e
searchPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: StringTypedTripleGetter e
    f = DTG.genericContext >-> rolgetter >-> searchProperty p

searchUnqualifiedPropertyOnContext :: forall e. StringTypedTripleGetter e  -> Id.LocalName -> StringTypedTripleGetter e
searchUnqualifiedPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: StringTypedTripleGetter e
    f = DTG.genericContext >-> rolgetter >-> searchUnqualifiedProperty p

searchExternalProperty :: forall e. String -> StringTypedTripleGetter e
searchExternalProperty pn = searchPropertyOnContext (typeWithPerspectivesTypes DTG.buitenRol) pn

searchExternalUnqualifiedProperty :: forall e. Id.LocalName -> StringTypedTripleGetter e
searchExternalUnqualifiedProperty ln = searchUnqualifiedPropertyOnContext (typeWithPerspectivesTypes DTG.buitenRol) ln

getInternalProperty :: forall e. String -> StringTypedTripleGetter e
getInternalProperty pn = typeWithPerspectivesTypes DTG.binnenRol >-> genericGetProperty pn `trackedAs` pn

searchInternalUnqualifiedProperty :: forall e. Id.LocalName -> StringTypedTripleGetter e
searchInternalUnqualifiedProperty ln = typeWithPerspectivesTypes DTG.binnenRol >-> searchUnqualifiedProperty ln

getGebondenAls :: forall e. String -> StringTypedTripleGetter e
getGebondenAls rname = OGC.genericGetGebondenAls rname `trackedAs` rname

-----------------------------------------------------------
-- GET A PROPERTYDEFINITION FROM A ROL DEFINITION
-----------------------------------------------------------
getUnqualifiedPropertyDefinition ::	forall e. Id.LocalName -> StringTypedTripleGetter e
getUnqualifiedPropertyDefinition ln = (filter_
  (flip Id.hasLocalName ln)
  ("hasLocalName_" <> ln)
  (getUnqualifiedContextRol "rolInContext" >-> DTG.genericBinding >-> DTG.genericContext))

searchUnqualifiedPropertyDefinition ::	forall e. Id.LocalName -> StringTypedTripleGetter e
searchUnqualifiedPropertyDefinition ln = searchInAspectsAndPrototypes f
  where
    f :: StringTypedTripleGetter e
    f = DTG.genericContext >-> getUnqualifiedPropertyDefinition ln

-----------------------------------------------------------
-- INVERSE ROL
-----------------------------------------------------------
constructInverseRolGetter :: forall e.
  String ->
  StringTypedTripleGetter e
constructInverseRolGetter pn = (OGC.genericGetGebondenAls pn) `trackedAs` (pn <> "_inverse")

-- | The PropertyReferences of the View. Again, the typing is imprecise.
propertyReferenties :: forall e. StringTypedTripleGetter e
propertyReferenties = typeWithPerspectivesTypes searchUnqualifiedRolDefinition "propertyReferentie"
