module Perspectives.TripleGetterConstructors where

import Control.Plus (empty)
import Data.Array (elemIndex, union, difference, cons, nub, foldMap) as Arr
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF, unwrap)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (type (**>), MonadPerspectivesQuery, Triple(..), TripleGetter, TypedTripleGetter(..), MPQ, (@@))
import Perspectives.DataTypeTripleGetters (binding, buitenRol, genericBinding, context) as DTG
import Perspectives.DataTypeTripleGetters (binnenRol, identity)
import Perspectives.Identifiers (LocalName, hasLocalName) as Id
import Perspectives.ObjectGetterConstructors (directAspectProperties, directAspectRoles, getContextRol, getUnqualifiedContextRol, getRoleBinders, getUnqualifiedRoleBinders, agreesWithType, alternatives, localAspects) as OGC
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, AnyDefinition, BuitenRol, ContextDef(..), ContextRol, PBool(..), PropertyDef(..), RolDef(..), RolInContext, Value, getProperty, getUnqualifiedProperty, typeWithPerspectivesTypes)
import Perspectives.QueryCombinators (filter_)
import Perspectives.TripleAdministration (getRef, memorize)
import Perspectives.TripleGetterComposition (before, composeMonoidal, followedBy, preferLeft, (>->))
import Perspectives.TripleGetterFromObjectGetter (trackedAs)
import Prelude (class Eq, class Ord, class Show, bind, const, flip, join, map, pure, show, ($), (<>), (==), (>>=), (>>>))

-----------------------------------------------------------
-- COMBINATORS
-----------------------------------------------------------
-- | The recursive closure of a query, bottoming out when it has no results.
-- | The result only contains the argument id if it can be obtained by applying p,
-- | never because it is the root of the computation.
-- Test.Perspectives.TripleGetterConstructors, via closureOfBinding.
closure :: forall o.
  Eq o =>
  Ord o =>
  (o **> o) ->
  (o **> o)
closure (TypedTripleGetter nameOfp p) =
  memorize (getter []) name
  where
    getter :: Array o -> o -> MonadPerspectivesQuery (Triple o o)
    getter cumulator id = do
      t@(Triple{object : objectsOfP}) <- p id
      case Arr.elemIndex id cumulator of
        Nothing -> do
          (triples :: Array (Triple o o)) <- (traverse (getter (Arr.cons id cumulator))) (Arr.difference objectsOfP cumulator)
          objects <- pure $ Arr.nub $ join $ map (\(Triple{object}) -> object) triples
          pure $ Triple { subject: id
                        , predicate : name
                        , object: Arr.union objectsOfP objects
                        , dependencies : []
                        , supports : map (typeWithPerspectivesTypes getRef) (Arr.cons t triples)
                        , tripleGetter : getter []}
        otherwise -> pure t

    name :: String
    name = "closure(" <>  nameOfp <> ")"

-- | The recursive closure of a query, bottoming out when it has no results.
-- | The result contains the root.
-- Test.Perspectives.ModelBasedTripleGetters, via propertiesDef.
-- Test.Perspectives.TripleGetterConstructors
closure_ :: forall o.
  Show o =>
  Eq o =>
  Ord o =>
  (o **> o) ->
  (o **> o)
closure_ tg = concat identity (closure tg)

-- | Combinator to make a TripleGetter fail if it returns PBool "false".
-- | Useful in combination with computing alternatives using <|>
unlessFalse :: forall s. (s **> PBool) -> TripleGetter s PBool
unlessFalse tg id = (id @@ tg) >>= \r@(Triple{object}) -> case (Arr.elemIndex (PBool "false") object) of
  Nothing -> pure r
  otherwise -> empty

searchInRolTelescope :: (String **> String) -> (String **> String)
-- Test.Perspectives.TripleGetterConstructors, via searchProperty
searchInRolTelescope getter = (getter `preferLeft` \_ -> (DTG.genericBinding >-> searchInRolTelescope getter)) "searchInRolTelescope"

-- | Applies the TypedTripleGetter to each higher prototype until it succeeds or there is no prototype.
-- | Does *not* apply the getter to the ContextType that is passed in!
-- Test.Perspectives.TripleGetterConstructors
searchInPrototypeHierarchy :: forall o.
  Eq o =>
  Ord o =>
  (BuitenRol **> o) ->
  (AnyContext **> o)
searchInPrototypeHierarchy getter = DTG.buitenRol >-> typeWithPerspectivesTypes searchInRolTelescope getter

-- TODO hier ontstaat een loop.
-- Test.Perspectives.ModelBasedTripleGetters, via buitenRolBeschrijvingDef
searchLocallyAndInPrototypeHierarchy :: forall o.
  Eq o =>
  Ord o =>
  (AnyContext **> o) ->
  (AnyContext **> o)
searchLocallyAndInPrototypeHierarchy getter = (getter `preferLeft` \_ -> (DTG.buitenRol >-> DTG.binding >-> DTG.context >-> searchLocallyAndInPrototypeHierarchy getter)) "searchLocallyAndInPrototypeHierarchy"

-- | Applies the getter (s ~~> o) to the ContextType and all its prototypes and recursively to all its aspects.
-- Test.Perspectives.ModelBasedTripleGetters, via buitenRolBeschrijvingDef
searchInAspectsAndPrototypes :: forall o.
  Eq o =>
  Ord o =>
  (AnyContext **> o) ->
  (AnyContext **> o)
searchInAspectsAndPrototypes getter = ((searchLocallyAndInPrototypeHierarchy getter) `preferLeft` \_ -> (directAspects >-> searchInAspectsAndPrototypes getter)) "searchInAspectsAndPrototypes"

-- | Applies the getter (s **> o) to the RolDef and all its prototypes and recursively to all its aspects.
-- Test.Perspectives.TripleGetterConstructors (also via searchUnqualifiedPropertyDefinition).
searchInAspectRolesAndPrototypes :: forall o.
  Eq o =>
  Ord o =>
  (AnyContext **> o) ->
  (AnyContext **> o)
searchInAspectRolesAndPrototypes getter = ((searchLocallyAndInPrototypeHierarchy getter) `preferLeft` \_ -> (RolDef `before` directAspectRoles >-> unwrap `before` searchInAspectRolesAndPrototypes getter)) "searchInAspectRolesAndPrototypes"

-- | Applies the getter (s **> o) to the RolDef and all its prototypes and recursively to all its aspects.
-- Test.Perspectives.TripleGetterConstructors via searchUnqualifiedPropertyDefinition.
searchInAspectPropertiesAndPrototypes :: forall o.
  Eq o =>
  Ord o =>
  (AnyContext **> o) ->
  (AnyContext **> o)
searchInAspectPropertiesAndPrototypes getter = ((searchLocallyAndInPrototypeHierarchy getter) `preferLeft` \_ -> (PropertyDef `before` directAspectProperties >-> unwrap `before` searchInAspectPropertiesAndPrototypes getter)) "searchInAspectPropertiesAndPrototypes"

-- Test.Perspectives.TripleGetterConstructors
localAspects :: (AnyContext **> AnyContext)
localAspects = OGC.localAspects `trackedAs` "model:Perspectives$Context$directAspects"

directAspects :: (AnyContext **> AnyContext)
directAspects = searchLocallyAndInPrototypeHierarchy localAspects

-- Test.Perspectives.TripleGetterConstructors
directAspectRoles :: (RolDef **> RolDef)
directAspectRoles = OGC.directAspectRoles `trackedAs` "model:Perspectives$Rol$directAspectRoles"

directAspectProperties :: (PropertyDef **> PropertyDef)
directAspectProperties = OGC.directAspectProperties `trackedAs` "model:Perspectives$Property$directAspectProperties"

-- The concatenation of the results of two queries applied to the same origin.
-- Test.Perspectives.TripleGetterConstructors
concat :: forall s o.
  Eq o =>
  Ord o =>
  (s **> o) ->
  (s **> o) ->
  (s **> o)
concat (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter s o
    getter id = do
      pt@(Triple{object : ps}) <- p id
      qt@(Triple{object : qs}) <- q id
      pure $ Triple { subject: id
                    , predicate : name
                    , object : (Arr.union ps qs)
                    , dependencies : []
                    , supports : map (typeWithPerspectivesTypes getRef) [pt, qt]
                    , tripleGetter : getter}

    name = "(concat " <> nameOfp <> " " <> nameOfq <> ")"

agreesWithType :: AnyDefinition -> (AnyDefinition **> PBool)
agreesWithType t = OGC.agreesWithType t `trackedAs` ("agreesWithType_" <> t)

alternatives :: (AnyContext **> AnyContext)
alternatives = OGC.alternatives `trackedAs` "alternatives"

-- | True iff at least one of the boolean results of f is true (where true is represented as PBool "true"). Yields false when applied to
-- an empty sequence.
-- Test.Perspectives.TripleGetterConstructors
some :: forall s. (s **> PBool) -> (s **> PBool)
some f = composeMonoidal f (alaF Disj Arr.foldMap ((==) (PBool "true")) >>> show >>> PBool) "some"

-- | True iff at all of the boolean results of f is true (where true is represented as PBool "true"). Yields true when applied to
-- an empty sequence.
-- Test.Perspectives.TripleGetterConstructors
all :: forall s. (s **> PBool) -> (s **> PBool)
all f = composeMonoidal f (alaF Conj Arr.foldMap ((==) (PBool "true")) >>> show >>> PBool) "all"

count :: forall s o. (s **> o) -> (s **> Int)
count f = composeMonoidal f (alaF Additive Arr.foldMap (const 1) ) "count"

-----------------------------------------------------------
-- CLOSURES
-----------------------------------------------------------
-- | All role instances in the telescope, excluding the root.
-- | A closure must be homogeneously typed. Here we require that the members of the collection are instances
-- | of the class Binding.
closureOfBinding :: forall r. Ord r => Binding r r => (r **> r)
closureOfBinding = closure DTG.binding

-- | All Aspects of a ContextType, excluding the ContextType itself. A homogeneous collection of Aspects, which
-- | can be any definition.
closureOfAspect :: (AnyDefinition **> AnyDefinition)
closureOfAspect = closure directAspects

-- | All AspectRollen of a RolDef, excluding the RolDef itself. A homogeneous collection of RolDefs.
closureOfAspectRol :: (RolDef **> RolDef)
closureOfAspectRol = closure directAspectRoles

-- | All AspectProperty of a PropertyDef, excluding the PropertyDef itself. A homogeneous collection of PropertyDefs.
closureOfAspectProperty :: (PropertyDef **> PropertyDef)
closureOfAspectProperty = closure directAspectProperties

-- | The prototype of a ContextType.
getPrototype :: (AnyDefinition **> AnyDefinition)
getPrototype = (DTG.buitenRol >-> DTG.binding >-> DTG.context)

-- | All prototypes of a ContextType, excluding the ContextType itself. A homogeneous collection of AnyDefinition.
closureOfPrototype :: (AnyDefinition **> AnyDefinition)
closureOfPrototype = closure getPrototype

-----------------------------------------------------------
-- GET A ROL FROM A CONTEXT
-----------------------------------------------------------
-- | Get the ContextRol instances with the given rol name (RolDef) directly from the Context definition (not searching prototypes or Aspects).
-- | E.g. getRol "model:Perspectives$View$rolProperty" will return all rol instances that bind a PropertyDef on an instance of psp:View.
-- Test.Perspectives.TripleGetterConstructors via getRolinContext
getContextRol :: RolDef -> (AnyContext **> ContextRol)
getContextRol rn = OGC.getContextRol rn `trackedAs` unwrap rn

-- | As getContextRol, but for RolinContext (same function, differently typed).
-- Test.Perspectives.TripleGetterConstructors
getRolInContext :: RolDef -> (AnyContext **> RolInContext)
getRolInContext = typeWithPerspectivesTypes getContextRol


-- | Get the ContextRol instances with the given local name directly from the Context.
-- E.g. getUnqualifiedContextRol "rolProperty" will return the same result as getContextRol
-- "model:Perspectives$View$rolProperty".
-- Test.Perspectives.TripleGetterConstructors via getUnqualifiedRolInContext
getUnqualifiedContextRol :: Id.LocalName -> (AnyContext **> ContextRol)
getUnqualifiedContextRol ln = OGC.getUnqualifiedContextRol ln `trackedAs` ln

-- | As getUnqualifiedContextRol, but for RolinContext (same function, differently typed).
-- Test.Perspectives.TripleGetterConstructors
getUnqualifiedRolInContext :: Id.LocalName -> (AnyContext **> RolInContext)
getUnqualifiedRolInContext = typeWithPerspectivesTypes getUnqualifiedContextRol

-- | This query constructor takes a context id as argument. The query step that results can be applied to a role definition
-- | and will result in all instances of that role for the given context.
-- | For domain we just take AnyContext. Range can only be psp:Rol because we have no
-- | other knowledge on it.
-- | psp:ContextInstance -> psp:Function
rolesOf :: AnyContext -> (RolDef **> RolInContext)
rolesOf cid = TypedTripleGetter ("rolesOf_" <> cid) (typeWithPerspectivesTypes getter) where
  getter :: RolDef -> MPQ (Triple AnyContext RolInContext)
  getter rd = cid @@ searchRolInContext rd

-----------------------------------------------------------
-- SEARCH A ROL IN A CONTEXT AND ITS PROTOTYPES
-----------------------------------------------------------
-- | Search for a qualified ContextRol both in the local context and all its prototypes.
-- Test.Perspectives.TripleGetterConstructors
searchContextRol :: RolDef -> (AnyContext **> ContextRol)
searchContextRol rn = searchLocallyAndInPrototypeHierarchy ((getContextRol rn) :: (AnyContext **> ContextRol))

-- | Search for a qualified ContextRol both in the local context and all its prototypes.
searchRolInContext :: RolDef -> (AnyContext **> RolInContext)
searchRolInContext rn = searchLocallyAndInPrototypeHierarchy ((getRolInContext rn) :: (AnyContext **> RolInContext))

-- | Search for an unqualified rol both in the local context and all its prototypes.
-- Test.Perspectives.TripleGetterConstructors
searchUnqualifiedRol :: Id.LocalName -> (AnyContext **> ContextRol)
searchUnqualifiedRol rn = searchLocallyAndInPrototypeHierarchy ( (getUnqualifiedContextRol rn) :: (AnyContext **> ContextRol))

-----------------------------------------------------------
-- GET A ROLDEFINITION FROM A CONTEXT DEFINITION
-----------------------------------------------------------
-- When looking for a Rol on a context, we can use both its local and qualified name. Either way we have to look in the
-- index of roles in that context.
-- However, when looking for a Rol **definition**, once we have a qualified name, we have the definition itself, as it
-- is represented by a context and we retrieve that directly from the database. Hence, all functions that extract a
-- Rol definition from a Context definition work with unqualified names.

-- | Look for the definition of a Rol by its local name, in the ContextDef (not searching prototypes or Aspects).
-- | If no Rol is defined with this local name, will return an empty result.
-- Test.Perspectives.TripleGetterConstructors
getUnqualifiedRolDefinition ::  Id.LocalName -> (ContextDef **> RolDef)
getUnqualifiedRolDefinition ln = unwrap `before` (filter_
  (flip Id.hasLocalName ln)
  ("hasLocalName_" <> ln)
  (getUnqualifiedContextRol "rolInContext" >-> DTG.binding >-> DTG.context))
    `followedBy` RolDef

-- | Look for the definition of a Rol by its local name, in the ContextDef and its Aspects and in all their prototypes.
-- | As the name of a RolDefinition on an Aspect will be scoped to that Aspect, we do not have to search once
-- | we have the qualified name of the Rol. Hence there is no version that searches qualified roles!
-- Test.Perspectives.TripleGetterConstructors
searchUnqualifiedRolDefinition ::	Id.LocalName -> (ContextDef **> RolDef)
searchUnqualifiedRolDefinition ln = typeWithPerspectivesTypes $ searchInAspectsAndPrototypes f
  where
    f :: (AnyContext **> RolDef)
    f = ContextDef `before` getUnqualifiedRolDefinition ln

-----------------------------------------------------------
-- GET A PROPERTY FROM A ROLE TELESCOPE
-----------------------------------------------------------
-- | The value of the property pd, wherever in the telescope it is represented.
-- Test.Perspectives.TripleGetterConstructors
searchProperty :: forall b. RolClass b => PropertyDef -> (b **> Value)
searchProperty pd = typeWithPerspectivesTypes searchInRolTelescope g
  where
    g :: (b **> Value)
    g = getProperty pd `trackedAs` (unwrap pd)

-- | The value of the unqualified property pd, wherever in the telescope it is represented.
-- Test.Perspectives.TripleGetterConstructors
searchUnqualifiedProperty :: forall b. RolClass b => Id.LocalName -> (b **> Value)
searchUnqualifiedProperty pd = typeWithPerspectivesTypes searchInRolTelescope g
  where
    g :: (b **> Value)
    g = getUnqualifiedProperty pd `trackedAs` pd

-----------------------------------------------------------
-- GET A PROPERTY FROM A CONTEXT
-----------------------------------------------------------
-- | This is especially useful for the Binnen- and BuitenRol. E.g. it allows us to have a context with an external
-- | property that shadows the value of that property on the prototype of the context.

-- | Searches the qualified property first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
-- Test.Perspectives.TripleGetterConstructors via searchExternalProperty
searchPropertyOnContext :: forall r. RolClass r => (AnyContext **> r) -> PropertyDef -> (AnyContext **> Value)
searchPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (AnyContext **> Value)
    f = (rolgetter >-> searchProperty p)

-- | Searches the property with the local name first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
-- Test.Perspectives.TripleGetterConstructors via searchExternalUnqualifiedProperty
searchUnqualifiedPropertyOnContext :: forall r. RolClass r => (AnyContext **> r)  -> Id.LocalName -> (AnyContext **> Value)
searchUnqualifiedPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (AnyContext **> Value)
    f = (rolgetter >-> searchUnqualifiedProperty p)

-- | Look for the property PropertyDef on the buitenRol of the ContextType c and on its telescope, shadowing any values
-- | on the prototypes.
-- Test.Perspectives.TripleGetterConstructors
searchExternalProperty :: PropertyDef -> (AnyContext **> Value)
searchExternalProperty pn = searchPropertyOnContext DTG.buitenRol pn

-- | Look for the property with the given local name on the buitenRol of the ContextType c and on its telescope,
-- | shadowing any values on the prototypes.
-- Test.Perspectives.TripleGetterConstructors
searchExternalUnqualifiedProperty :: Id.LocalName -> (AnyContext **> Value)
searchExternalUnqualifiedProperty ln = searchUnqualifiedPropertyOnContext DTG.buitenRol ln

-- | Look for the property with the given local name on the binnenRol of the ContextType c.
-- Test.Perspectives.TripleGetterConstructors
getInternalProperty :: PropertyDef -> (AnyContext **> Value)
getInternalProperty pn = binnenRol >-> getProperty pn `trackedAs` (unwrap pn)

-- Test.Perspectives.TripleGetterConstructors via searchInternalUnqualifiedProperty
getInternalUnqualifiedProperty :: Id.LocalName -> (AnyContext **> Value)
getInternalUnqualifiedProperty ln = binnenRol >-> getUnqualifiedProperty ln `trackedAs` ln

-- | Look for the property with the given local name on the binnenRol of the ContextType c and on its telescope.
-- Test.Perspectives.TripleGetterConstructors
searchInternalUnqualifiedProperty :: Id.LocalName -> (AnyContext **> Value)
searchInternalUnqualifiedProperty ln = ((getInternalUnqualifiedProperty ln) `preferLeft` \_ -> (searchExternalUnqualifiedProperty ln)) "searchInternalUnqualifiedProperty"

-- | From the instance of a Rol of any kind, find the instances of the Rol of the given type that bind it (that have
-- | it as their binding).
-- Test.Perspectives.TripleGetterConstructors
getRoleBinders :: forall r b. RolClass r => RolClass b => RolDef -> (r **> b)
getRoleBinders rname = OGC.getRoleBinders rname `trackedAs` (unwrap rname)

-- | From the instance of a Rol of any kind, find the instances of the Rol with the given local name
-- | that bind it (that have it as their binding). The type of ln can be 'buitenRolBeschrijving'.
-- Test.Perspectives.TripleGetterConstructors
getUnqualifiedRoleBinders :: forall r b. RolClass r => RolClass b => Id.LocalName -> (r **> b)
getUnqualifiedRoleBinders rname = OGC.getUnqualifiedRoleBinders rname `trackedAs` rname
-----------------------------------------------------------
-- GET A PROPERTYDEFINITION FROM A ROL DEFINITION
-----------------------------------------------------------
-- When looking for a Property (value) on a Rol, we can use both its local and qualified name. Either way we have to
-- look in the index of properties in that role.
-- However, when looking for a Property **definition**, once we have a qualified name, we have the definition itself,
-- as it is represented by a context and we retrieve that directly from the database. Hence, all functions that extract
-- a Property definition from a Rol definition work with unqualified names.

-- | Look for the definition of a Property by its local name, in the RolDef (not searching prototypes or Aspects).
-- | If no Property is defined with this local name, will return an empty result.
-- Test.Perspectives.TripleGetterConstructors
getUnqualifiedPropertyDefinition ::	Id.LocalName -> (RolDef **> PropertyDef)
getUnqualifiedPropertyDefinition ln = unwrap `before` (filter_
  (flip Id.hasLocalName ln)
  ("hasLocalName_" <> ln)
  (getUnqualifiedContextRol "rolProperty" >-> DTG.binding >-> DTG.context))
    `followedBy` PropertyDef

   -- typeWithPerspectivesTypes $ getUnqualifiedContextRol ln >-> DTG.binding >-> DTG.context

-- | Look for the definition of a Property by its local name, in the RolDef and its Aspects
-- | and in all their prototypes. Use
-- | Perspectives.ModelBasedTripleGetters.collectUnqualifiedPropertyDefinitions to include the
-- | rolGraph in the search.
searchUnqualifiedPropertyDefinition ::	Id.LocalName -> (RolDef **> PropertyDef)
searchUnqualifiedPropertyDefinition ln = unwrap `before` (searchInAspectRolesAndPrototypes f)
  where
    f :: (AnyContext **> PropertyDef)
    f = (RolDef `before` getUnqualifiedPropertyDefinition ln)
