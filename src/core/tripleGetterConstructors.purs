module Perspectives.TripleGetterConstructors where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (elemIndex, union, difference, cons, null, nub, foldMap) as Arr
import Data.Maybe (Maybe(..))
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF, unwrap)
import Data.Traversable (traverse)
import Perspectives.CoreTypes (type (**>), MonadPerspectivesQuery, Triple(..), TripleGetter, TypedTripleGetter(..), MPQ, (@@))
import Perspectives.DataTypeTripleGetters (binding, buitenRol, genericBinding, context) as DTG
import Perspectives.DataTypeTripleGetters (binnenRol, identity)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (LocalName, hasLocalName) as Id
import Perspectives.ObjectGetterConstructors (directAspectProperties, directAspectRoles, directAspects, getContextRol, getUnqualifiedContextRol, getRoleBinders) as OGC
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, AnyDefinition, BuitenRol, ContextDef(..), ContextRol, PBool(..), PropertyDef(..), RolDef(..), RolInContext, Value, getProperty, getUnqualifiedProperty, typeWithPerspectivesTypes)
import Perspectives.QueryCombinators (filter_)
import Perspectives.TripleAdministration (getRef, memorize)
import Perspectives.TripleGetterComposition (before, composeMonoidal, followedBy, (>->))
import Perspectives.TripleGetterFromObjectGetter (trackedAs)
import Prelude (class Eq, bind, pure, ($), (<>), join, map, (>>=), (>>>), (==), show, flip)

-----------------------------------------------------------
-- COMBINATORS
-----------------------------------------------------------
-- | The recursive closure of a query, bottoming out when it has no results.
-- | The result only contains the argument id if it can be obtained by applying p,
-- | never because it is the root of the computation.
-- Test.Perspectives.TripleGetterConstructors, via closureOfBinding.
closure :: forall o e.
  Eq o =>
  (o **> o) e ->
  (o **> o) e
closure (TypedTripleGetter nameOfp p) =
  memorize (getter []) name
  where
    getter :: Array o -> o -> MonadPerspectivesQuery (AjaxAvarCache e) (Triple o o e)
    getter cumulator id = do
      t@(Triple{object : objectsOfP}) <- p id
      case Arr.elemIndex id cumulator of
        Nothing -> do
          (triples :: Array (Triple o o e)) <- (traverse (getter (Arr.cons id cumulator))) (Arr.difference objectsOfP cumulator)
          objects <- pure $ Arr.nub $ join $ map (\(Triple{object}) -> object) triples
          pure $ Triple { subject: id
                        , predicate : name
                        , object: Arr.union objectsOfP objects
                        , dependencies : []
                        , supports : map (typeWithPerspectivesTypes getRef) (Arr.cons t triples)
                        , tripleGetter : getter []}
        otherwise -> pure t

    name :: String
    name = "(closure " <>  nameOfp <> ")"

-- | The recursive closure of a query, bottoming out when it has no results.
-- | The result contains the root.
-- Test.Perspectives.ModelBasedTripleGetters, via propertiesDef.
-- Test.Perspectives.TripleGetterConstructors
closure_ :: forall o e.
  Eq o =>
  (o **> o) e ->
  (o **> o) e
closure_ tg = concat identity (closure tg)

-- | Combinator to make an ObjectsGetter fail if it returns an empty result.
-- | Useful in combination with computing alternatives using <|>
unlessNull :: forall s o e. (s **> o) e -> TripleGetter s o e
unlessNull tg id = (id @@ tg) >>= \r@(Triple{object}) -> if (Arr.null object) then empty else pure r

searchInRolTelescope :: forall e. (String **> String) e -> (String **> String) e
-- Test.Perspectives.TripleGetterConstructors, via searchProperty
searchInRolTelescope getter@(TypedTripleGetter n _) = TypedTripleGetter n f where
  f :: TripleGetter String String e
  f rolId =
    unlessNull getter rolId
    <|>
    (rolId @@ (DTG.genericBinding >-> searchInRolTelescope getter))

-- | Applies the TypedTripleGetter to each higher prototype until it succeeds or there is no prototype.
-- | Does *not* apply the getter to the ContextType that is passed in!
-- Test.Perspectives.TripleGetterConstructors
searchInPrototypeHierarchy :: forall o e.
  Eq o =>
  (BuitenRol **> o) e ->
  (AnyContext **> o) e
searchInPrototypeHierarchy getter = DTG.buitenRol >-> typeWithPerspectivesTypes searchInRolTelescope getter

-- Test.Perspectives.ModelBasedTripleGetters, via buitenRolBeschrijvingDef
searchLocallyAndInPrototypeHierarchy :: forall o e.
  Eq o =>
  (AnyContext **> o) e ->
  (AnyContext **> o) e
searchLocallyAndInPrototypeHierarchy getter@(TypedTripleGetter n _) = TypedTripleGetter n f where
  f :: TripleGetter AnyContext o e
  f (cid :: AnyContext) =
    unlessNull getter cid
    <|>
    (cid @@ (DTG.buitenRol >-> DTG.binding >-> DTG.context >-> searchLocallyAndInPrototypeHierarchy getter))

-- | Applies the getter (s ~~> o) e to the ContextType and all its prototypes and recursively to all its aspects.
-- Test.Perspectives.ModelBasedTripleGetters, via buitenRolBeschrijvingDef
searchInAspectsAndPrototypes :: forall o e.
  Eq o =>
  (AnyContext **> o) e ->
  (AnyContext **> o) e
searchInAspectsAndPrototypes getter@(TypedTripleGetter n _) = TypedTripleGetter n f where
  f :: TripleGetter AnyContext o e
  f contextId =
    unlessNull (searchLocallyAndInPrototypeHierarchy getter) contextId
    <|>
    (contextId @@ (directAspects >-> searchInAspectsAndPrototypes getter))

-- | Applies the getter (s **> o) e to the RolDef and all its prototypes and recursively to all its aspects.
-- Test.Perspectives.TripleGetterConstructors via searchUnqualifiedPropertyDefinition.
searchInAspectRolesAndPrototypes :: forall o e.
  Eq o =>
  (AnyContext **> o) e ->
  (AnyContext **> o) e
searchInAspectRolesAndPrototypes getter@(TypedTripleGetter n _) = TypedTripleGetter n f where
  f :: TripleGetter AnyContext o e
  f rolDefId =
    unlessNull (searchLocallyAndInPrototypeHierarchy getter) rolDefId
    <|>
    (rolDefId @@ (RolDef `before` directAspectRoles >-> unwrap `before` searchInAspectRolesAndPrototypes getter))

-- Test.Perspectives.TripleGetterConstructors
directAspects :: forall e. (AnyContext **> AnyContext) e
directAspects = OGC.directAspects `trackedAs` "model:Perspectives$Context$directAspects"

-- Test.Perspectives.TripleGetterConstructors
directAspectRoles :: forall e. (RolDef **> RolDef) e
directAspectRoles = OGC.directAspectRoles `trackedAs` "model:Perspectives$Rol$directAspectRoles"

directAspectProperties :: forall e. (PropertyDef **> PropertyDef) e
directAspectProperties = OGC.directAspectProperties `trackedAs` "model:Perspectives$Property$directAspectProperties"

-- The concatenation of the results of two queries applied to the same origin.
-- Test.Perspectives.TripleGetterConstructors
concat :: forall s o e.
  Eq o =>
  (s **> o) e ->
  (s **> o) e ->
  (s **> o) e
concat (TypedTripleGetter nameOfp p) (TypedTripleGetter nameOfq q) = do
  memorize getter name
  where
    getter :: TripleGetter s o e
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

-- | True iff at least one of the boolean results of f is true (where true is represented as PBool "true").
-- Test.Perspectives.TripleGetterConstructors
some :: forall s e. (s **> PBool) e -> (s **> PBool) e
some f = composeMonoidal f (alaF Disj Arr.foldMap ((==) (PBool "true")) >>> show >>> PBool) "some"

-- | True iff at all of the boolean results of f is true (where true is represented as PBool "true").
-- Test.Perspectives.TripleGetterConstructors
all :: forall s e. (s **> PBool) e -> (s **> PBool) e
all f = composeMonoidal f (alaF Conj Arr.foldMap ((==) (PBool "true")) >>> show >>> PBool) "all"

-----------------------------------------------------------
-- CLOSURES
-----------------------------------------------------------
-- | All role instances in the telescope, excluding the root.
-- | A closure must be homogeneously typed. Here we require that the members of the collection are instances
-- | of the class Binding.
closureOfBinding :: forall r e. Binding r r => (r **> r) e
closureOfBinding = closure DTG.binding

-- | All Aspects of a ContextType, excluding the ContextType itself. A homogeneous collection of Aspects, which
-- | can be any definition.
closureOfAspect :: forall e. (AnyDefinition **> AnyDefinition) e
closureOfAspect = closure directAspects

-- | All AspectRollen of a RolDef, excluding the RolDef itself. A homogeneous collection of RolDefs.
closureOfAspectRol :: forall e. (RolDef **> RolDef) e
closureOfAspectRol = closure directAspectRoles

-- | All AspectProperty of a PropertyDef, excluding the PropertyDef itself. A homogeneous collection of PropertyDefs.
closureOfAspectProperty :: forall e. (PropertyDef **> PropertyDef) e
closureOfAspectProperty = closure directAspectProperties

-- | The prototype of a ContextType.
getPrototype :: forall e. (AnyDefinition **> AnyDefinition) e
getPrototype = (DTG.buitenRol >-> DTG.binding >-> DTG.context)

-- | All prototypes of a ContextType, excluding the ContextType itself. A homogeneous collection of AnyDefinition.
closureOfPrototype :: forall e. (AnyDefinition **> AnyDefinition) e
closureOfPrototype = closure getPrototype

-----------------------------------------------------------
-- GET A ROL FROM A CONTEXT
-----------------------------------------------------------
-- | Get the ContextRol instances with the given rol name (RolDef) directly from the Context definition (not searching prototypes or Aspects).
-- | E.g. getRol "model:Perspectives$View$rolProperty" will return all rol instances that bind a PropertyDef on an instance of psp:View.
-- Test.Perspectives.TripleGetterConstructors via getRolinContext
getContextRol :: forall e. RolDef -> (AnyContext **> ContextRol) e
getContextRol rn = OGC.getContextRol rn `trackedAs` unwrap rn

-- | As getContextRol, but for RolinContext (same function, differently typed).
-- Test.Perspectives.TripleGetterConstructors
getRolInContext :: forall e. RolDef -> (AnyContext **> RolInContext) e
getRolInContext = typeWithPerspectivesTypes getContextRol


-- | Get the ContextRol instances with the given local name directly from the Context.
-- E.g. getUnqualifiedContextRol "rolProperty" will return the same result as getContextRol
-- "model:Perspectives$View$rolProperty".
-- Test.Perspectives.TripleGetterConstructors via getUnqualifiedRolInContext
getUnqualifiedContextRol :: forall e. Id.LocalName -> (AnyContext **> ContextRol) e
getUnqualifiedContextRol ln = OGC.getUnqualifiedContextRol ln `trackedAs` ln

-- | As getUnqualifiedContextRol, but for RolinContext (same function, differently typed).
-- Test.Perspectives.TripleGetterConstructors
getUnqualifiedRolInContext :: forall e. Id.LocalName -> (AnyContext **> RolInContext) e
getUnqualifiedRolInContext = typeWithPerspectivesTypes getUnqualifiedContextRol

-- | This query constructor takes a context id as argument. The query step that results can be applied to a role definition
-- | and will result in all instances of that role for the given context.
-- | For domain we just take AnyContext. Range can only be psp:Rol because we have no
-- | other knowledge on it.
-- | psp:ContextInstance -> psp:Function
rolesOf :: forall e. AnyContext -> (RolDef **> RolInContext) e
rolesOf cid = TypedTripleGetter ("rolesOf_" <> cid) (typeWithPerspectivesTypes getter) where
  getter :: RolDef -> MPQ e (Triple AnyContext RolInContext e)
  getter rd = cid @@ searchRolInContext rd

-----------------------------------------------------------
-- SEARCH A ROL IN A CONTEXT AND ITS PROTOTYPES
-----------------------------------------------------------
-- | Search for a qualified ContextRol both in the local context and all its prototypes.
-- Test.Perspectives.TripleGetterConstructors
searchContextRol :: forall e. RolDef -> (AnyContext **> ContextRol) e
searchContextRol rn = searchLocallyAndInPrototypeHierarchy ((getContextRol rn) :: (AnyContext **> ContextRol) e)

-- | Search for a qualified ContextRol both in the local context and all its prototypes.
searchRolInContext :: forall e. RolDef -> (AnyContext **> RolInContext) e
searchRolInContext rn = searchLocallyAndInPrototypeHierarchy ((getRolInContext rn) :: (AnyContext **> RolInContext) e)

-- | Search for an unqualified rol both in the local context and all its prototypes.
-- TODO: hernoem getRolFromPrototypeHierarchy naar searchUnqualifiedRol
-- OF: let op of niet searchRolDefinitionInAspects gebruikt moet worden (mogelijke fout in aanroepende code!)
-- TODO: waarom alleen in ContextDef?
-- Test.Perspectives.TripleGetterConstructors
searchUnqualifiedRol :: forall e. Id.LocalName -> (AnyContext **> ContextRol) e
searchUnqualifiedRol rn = searchLocallyAndInPrototypeHierarchy ( (getUnqualifiedContextRol rn) :: (AnyContext **> ContextRol) e)

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
getUnqualifiedRolDefinition ::	forall e. Id.LocalName -> (ContextDef **> RolDef) e
getUnqualifiedRolDefinition ln = unwrap `before` (filter_
  (flip Id.hasLocalName ln)
  ("hasLocalName_" <> ln)
  (getUnqualifiedContextRol "rolInContext" >-> DTG.binding >-> DTG.context))
    `followedBy` RolDef

-- | Look for the definition of a Rol by its local name, in the ContextDef and its Aspects and in all their prototypes.
-- | As the name of a RolDefinition on an Aspect will be scoped to that Aspect, we do not have to search once
-- | we have the qualified name of the Rol. Hence there is no version that searches qualified roles!
-- Test.Perspectives.TripleGetterConstructors
searchUnqualifiedRolDefinition ::	forall e. Id.LocalName -> (ContextDef **> RolDef) e
searchUnqualifiedRolDefinition ln = typeWithPerspectivesTypes $ searchInAspectsAndPrototypes f
  where
    f :: (AnyContext **> RolDef) e
    f = ContextDef `before` getUnqualifiedRolDefinition ln

-----------------------------------------------------------
-- GET A PROPERTY FROM A ROLE TELESCOPE
-----------------------------------------------------------
-- | The value of the property pd, wherever in the telescope it is represented.
-- Test.Perspectives.TripleGetterConstructors
searchProperty :: forall b e. RolClass b => PropertyDef -> (b **> Value) e
searchProperty pd = typeWithPerspectivesTypes searchInRolTelescope g
  where
    g :: (b **> Value) e
    g = getProperty pd `trackedAs` (unwrap pd)

-- | The value of the unqualified property pd, wherever in the telescope it is represented.
-- | NOTE: this function cannot be applied to a BinnenRol.
-- Test.Perspectives.TripleGetterConstructors
searchUnqualifiedProperty :: forall b e. RolClass b => Id.LocalName -> (b **> Value) e
searchUnqualifiedProperty pd = typeWithPerspectivesTypes searchInRolTelescope g
  where
    g :: (b **> Value) e
    g = getUnqualifiedProperty pd `trackedAs` pd

-----------------------------------------------------------
-- GET A PROPERTY FROM A CONTEXT
-----------------------------------------------------------
-- | This is especially useful for the Binnen- and BuitenRol. E.g. it allows us to have a context with an external
-- | property that shadows the value of that property on the prototype of the context.

-- | Searches the qualified property first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
-- Test.Perspectives.TripleGetterConstructors via searchExternalProperty
searchPropertyOnContext :: forall r e. RolClass r => (AnyContext **> r) e -> PropertyDef -> (AnyContext **> Value) e
searchPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (AnyContext **> Value) e
    f = (rolgetter >-> searchProperty p)

-- | Searches the property with the local name first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
-- Test.Perspectives.TripleGetterConstructors via searchExternalUnqualifiedProperty
searchUnqualifiedPropertyOnContext :: forall r e. RolClass r => (AnyContext **> r) e  -> Id.LocalName -> (AnyContext **> Value) e
searchUnqualifiedPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (AnyContext **> Value) e
    f = (rolgetter >-> searchUnqualifiedProperty p)

-- | Look for the property PropertyDef on the buitenRol of the ContextType c and on its telescope, shadowing any values
-- | on the prototypes.
-- Test.Perspectives.TripleGetterConstructors
searchExternalProperty :: forall e. PropertyDef -> (AnyContext **> Value) e
searchExternalProperty pn = searchPropertyOnContext DTG.buitenRol pn

-- | Look for the property with the given local name on the buitenRol of the ContextType c and on its telescope,
-- | shadowing any values on the prototypes.
-- Test.Perspectives.TripleGetterConstructors
searchExternalUnqualifiedProperty :: forall e. Id.LocalName -> (AnyContext **> Value) e
searchExternalUnqualifiedProperty ln = searchUnqualifiedPropertyOnContext DTG.buitenRol ln

-- | Look for the property with the given local name on the binnenRol of the ContextType c.
-- Test.Perspectives.TripleGetterConstructors
getInternalProperty :: forall e. PropertyDef -> (AnyContext **> Value) e
getInternalProperty pn = binnenRol >-> getProperty pn `trackedAs` (unwrap pn)

-- Test.Perspectives.TripleGetterConstructors via searchInternalUnqualifiedProperty
getInternalUnqualifiedProperty :: forall e. Id.LocalName -> (AnyContext **> Value) e
getInternalUnqualifiedProperty ln = binnenRol >-> getUnqualifiedProperty ln `trackedAs` ln

-- | Look for the property with the given local name on the binnenRol of the ContextType c and on its telescope.
-- Test.Perspectives.TripleGetterConstructors
searchInternalUnqualifiedProperty :: forall e. Id.LocalName -> (AnyContext **> Value) e
searchInternalUnqualifiedProperty ln = TypedTripleGetter ln f
  where
  f :: TripleGetter AnyContext Value e
  f cid =
      unlessNull (getInternalUnqualifiedProperty ln) cid
      <|>
      (cid @@ (searchExternalUnqualifiedProperty ln))

-- | From the instance of a Rol of any kind, find the instances of the Rol of the given type that bind it (that have
-- | it as their binding).
-- Test.Perspectives.TripleGetterConstructors
getRoleBinders :: forall r b e. RolClass r => RolClass b => RolDef -> (r **> b) e
getRoleBinders rname = OGC.getRoleBinders rname `trackedAs` (unwrap rname)

-----------------------------------------------------------
-- GET A PROPERTYDEFINITION FROM A ROL DEFINITION
-----------------------------------------------------------
-- TODO: OOK DE BINDING DRAAGT PROPERTIES BIJ!
-- When looking for a Property (value) on a Rol, we can use both its local and qualified name. Either way we have to
-- look in the index of properties in that role.
-- However, when looking for a Property **definition**, once we have a qualified name, we have the definition itself,
-- as it is represented by a context and we retrieve that directly from the database. Hence, all functions that extract
-- a Property definition from a Rol definition work with unqualified names.

-- | Look for the definition of a Property by its local name, in the RolDef (not searching prototypes or Aspects).
-- | If no Property is defined with this local name, will return an empty result.
getUnqualifiedPropertyDefinition ::	forall e. Id.LocalName -> (RolDef **> PropertyDef) e
getUnqualifiedPropertyDefinition ln = unwrap `before` (filter_
  (flip Id.hasLocalName ln)
  ("hasLocalName_" <> ln)
  (getUnqualifiedContextRol "rolInContext" >-> DTG.binding >-> DTG.context))
    `followedBy` PropertyDef

   -- typeWithPerspectivesTypes $ getUnqualifiedContextRol ln >-> DTG.binding >-> DTG.context

-- | Look for the definition of a Property by its local name, in the RolDef and its Aspects and in all their prototypes.
searchUnqualifiedPropertyDefinition ::	forall e. Id.LocalName -> (RolDef **> PropertyDef) e
searchUnqualifiedPropertyDefinition ln = unwrap `before` (searchInAspectsAndPrototypes f)
  where
    f :: (AnyContext **> PropertyDef) e
    f = (RolDef `before` getUnqualifiedPropertyDefinition ln)
