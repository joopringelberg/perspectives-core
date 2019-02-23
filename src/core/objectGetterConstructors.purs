module Perspectives.ObjectGetterConstructors where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (cons, difference, elemIndex, foldMap, head, nub, null, union)
import Data.Array (filter) as Arr
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF, unwrap)
import Data.StrMap (lookup)
import Data.Traversable (traverse)
import Perspectives.ContextAndRole (context_rolInContext)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (type (~~>), MP, ObjectsGetter)
import Perspectives.DataTypeObjectGetters (binnenRol, buitenRol, context, rolBindingDef)
import Perspectives.Identifiers (LocalName, hasLocalName) as Id
import Perspectives.ObjectsGetterComposition (composeMonoidal, (/-/), (\-\))
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, BuitenRol, ContextDef(..), ContextRol, PBool(..), PropertyDef(..), RolDef(..), RolInContext, Value, AnyDefinition, binding, genericBinding, getProperty, getUnqualifiedProperty, typeWithPerspectivesTypes)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..))
import Prelude (class Eq, bind, flip, id, join, map, pure, show, ($), (<<<), (<>), (==), (>=>), (>>=), (>>>))

-- | This module only exports constructors that search roles or properties,
-- | in the space of prototypes, Aspects, AspectRoles and AspectProperties.

-----------------------------------------------------------
-- COMBINATORS
-----------------------------------------------------------
-- | The closure of an ObjectsGetter (cumulates results, excluding the root).
closure :: forall o e.
  Eq o =>
  (o ~~> o) e ->
  (o ~~> o) e
closure p = getter [] where
  getter :: Array o -> o -> MP e (Array o)
  getter cumulator id = do
    (objectsOfP :: Array o) <- p id
    case elemIndex id cumulator of
      Nothing -> do
        (results :: Array (Array o)) <- traverse (getter (union cumulator objectsOfP)) (difference objectsOfP cumulator)
        pure $ nub $ join (cons objectsOfP results)
      otherwise -> pure objectsOfP

-- | Combinator to make an ObjectsGetter fail if it returns an empty result.
-- | Useful in combination with computing alternatives using <|>
unlessNull :: forall s o e. (s ~~> o) e -> (s ~~> o) e
unlessNull og id = og id >>= \r -> if (null r) then empty else pure r

contains :: forall s o e. Eq o => o -> (s ~~> o) e -> (s ~~> PBool) e
contains o getter = getter >=> \(os :: Array o) -> case elemIndex o os of
  Nothing -> pure [PBool "false"]
  otherwise -> pure [PBool "true"]

filter_ :: forall s o e.
  (o -> Boolean) ->
  (s ~~> o) e ->
  (s ~~> o) e
filter_ criterium getter = getter >=> pure <<< Arr.filter criterium

toBoolean :: forall s e. (s ~~> PBool) e -> s -> MP e Boolean
toBoolean g = g >=> \(bs :: Array PBool) -> case head bs of
  Nothing -> pure true
  (Just b) -> pure (b == PBool "true")

searchInRolTelescope :: forall e. ObjectsGetter e -> ObjectsGetter e
searchInRolTelescope getter rolId =
  unlessNull getter rolId
  <|>
  (genericBinding /-/ searchInRolTelescope getter) rolId

-- | Applies the ObjectsGetter to each higher prototype until it succeeds or there is no prototype.
-- | Does *not* apply the getter to the ContextType that is passed in!
searchInPrototypeHierarchy :: forall o e.
  Eq o =>
  (BuitenRol ~~> o) e ->
  (AnyContext ~~> o) e
searchInPrototypeHierarchy getter = buitenRol /-/ typeWithPerspectivesTypes searchInRolTelescope getter

-- | Applies the getter (BuitenRol ~~> o) e to any context and all its prototypes.
searchLocallyAndInPrototypeHierarchy :: forall o e.
  Eq o =>
  (BuitenRol ~~> o) e ->
  (AnyContext ~~> o) e
searchLocallyAndInPrototypeHierarchy getter c = unlessNull f c <|> searchInPrototypeHierarchy getter c
  where
    f :: (AnyContext ~~> o) e
    f = buitenRol /-/ getter

-- | Applies the getter (s ~~> o) e to the ContextType and all its prototypes and recursively to all its aspects.
searchInAspectsAndPrototypes :: forall o e.
  Eq o =>
  (BuitenRol ~~> o) e ->
  (AnyContext ~~> o) e
searchInAspectsAndPrototypes getter contextId =
  unlessNull (searchLocallyAndInPrototypeHierarchy getter) contextId
  <|>
  (directAspects /-/ searchInAspectsAndPrototypes getter) contextId

directAspects :: forall e. (AnyContext ~~> AnyContext) e
directAspects = getContextRol (RolDef "model:Perspectives$Context$aspect") /-/ rolBindingDef

directAspectRoles :: forall e. (RolDef ~~> RolDef) e
directAspectRoles = typeWithPerspectivesTypes $ getContextRol (RolDef "model:Perspectives$Rol$aspectRol") /-/ rolBindingDef

directAspectProperties :: forall e. (PropertyDef ~~> PropertyDef) e
directAspectProperties = typeWithPerspectivesTypes $ getContextRol (RolDef "model:Perspectives$Rol$aspectProperty") /-/ rolBindingDef

-- | The type of Rol or Context that can be bound to the Rol.
mogelijkeBinding :: forall e. (RolDef ~~> AnyDefinition) e
mogelijkeBinding = unwrap >>> getContextRol (RolDef "model:Perspectives$Rol$mogelijkeBinding") /-/ binding /-/ context

-- | Get the alternatives of a Sum type, possibly none. I assume Sums have no prototypes, nor Aspects.
alternatives :: forall e. (AnyContext ~~> AnyContext) e
alternatives = (getContextRol (RolDef "model:Perspectives$Sum$alternative")) /-/ rolBindingDef

-- | Traverse the acyclic directed graph of mogelijkeBinding until the function f yields a result.
searchInMogelijkeBinding :: forall o e. Eq o => (RolDef ~~> o) e -> (RolDef ~~> o) e
searchInMogelijkeBinding f roldef =
  (unlessNull
    (((mogelijkeBinding /-/ alternatives) >=> pure <<< map RolDef) \-\ f)) roldef -- sum type
  <|>
  ((mogelijkeBinding >=> pure <<< map RolDef) /-/ searchInMogelijkeBinding f) roldef -- single type

concat :: forall s o e. Eq o => (s ~~> o) e -> (s ~~> o) e -> (s ~~> o) e
concat f p s = do
  fs <- f s
  ps <- p s
  pure $ union fs ps

-- | True iff at least one of the boolean results of f is true (where true is represented as PBool "true").
some :: forall s e. (s ~~> PBool) e -> (s ~~> PBool) e
some f = f `composeMonoidal` (alaF Disj foldMap ((==) (PBool "true")) >>> show >>> PBool)

-- | True iff at all of the boolean results of f is true (where true is represented as PBool "true").
all :: forall s e. (s ~~> PBool) e -> (s ~~> PBool) e
all f = f `composeMonoidal` (alaF Conj foldMap ((==) (PBool "true")) >>> show >>> PBool)

-----------------------------------------------------------
-- CLOSURES
-----------------------------------------------------------
-- | All role instances in the telescope, excluding the root.
-- | A closure must be homogeneously typed. Here we require that the members of the collection are instances
-- | of the class Binding.
closureOfBinding :: forall r e. Binding r r => (r ~~> r) e
closureOfBinding = closure binding

-- | All Aspects of a ContextType, excluding the ContextType itself. A homogeneous collection of Aspects, which
-- | can be any definition.
closureOfAspect :: forall e. (AnyDefinition ~~> AnyDefinition) e
closureOfAspect = closure directAspects

-- | All AspectRollen of a RolDef, excluding the RolDef itself. A homogeneous collection of RolDefs.
closureOfAspectRol :: forall e. (RolDef ~~> RolDef) e
closureOfAspectRol = closure directAspectRoles

-- | All AspectProperty of a PropertyDef, excluding the PropertyDef itself. A homogeneous collection of PropertyDefs.
closureOfAspectProperty :: forall e. (PropertyDef ~~> PropertyDef) e
closureOfAspectProperty = closure directAspectProperties

-- | The prototype of a ContextType.
getPrototype :: forall e. (AnyDefinition ~~> AnyDefinition) e
getPrototype = (buitenRol /-/ binding /-/ context)

-- | All prototypes of a ContextType, excluding the ContextType itself. A homogeneous collection of AnyDefinition.
closureOfPrototype :: forall e. (AnyDefinition ~~> AnyDefinition) e
closureOfPrototype = closure getPrototype

-----------------------------------------------------------
-- GET A ROL FROM A CONTEXT
-----------------------------------------------------------
-- | Get the ContextRol instances with the given rol name (RolDef) directly from the Context definition (not searching prototypes or Aspects).
-- | E.g. getRol "model:Perspectives$View$rolProperty" will return all rol instances that bind a PropertyDef on an instance of psp:View.
getContextRol :: forall e. RolDef -> (AnyContext ~~> ContextRol) e
getContextRol rn = typeWithPerspectivesTypes $ getContextMember \context -> maybe [] id (lookup (unwrap rn) (context_rolInContext context))

-- | As getContextRol, but for RolinContext (same function, differently typed).
getRolInContext :: forall e. RolDef -> (AnyContext ~~> RolInContext) e
getRolInContext = typeWithPerspectivesTypes getContextRol

-- | Get the ContextRol instances with the given local name directly from the Context.
-- E.g. getUnqualifiedContextRol "rolProperty" will return the same result as getContextRol
-- "model:Perspectives$View$rolProperty".
-- TODO: rename getRolByLocalName to getUnqualifiedRol.
getUnqualifiedContextRol :: forall e. Id.LocalName -> (AnyContext ~~> ContextRol) e
getUnqualifiedContextRol ln = typeWithPerspectivesTypes $ getContextMember \context -> maybe [] id (lookup (ln `qualifiedWith` context) (context_rolInContext context))
  where
    qualifiedWith :: Id.LocalName -> PerspectContext -> String
    qualifiedWith ln (PerspectContext {pspType}) = pspType <> "$" <> ln

-- | As getUnqualifiedContextRol, but for RolinContext (same function, differently typed).
getUnqualifiedRolInContext :: forall e. Id.LocalName -> (AnyContext ~~> RolInContext) e
getUnqualifiedRolInContext = typeWithPerspectivesTypes getUnqualifiedContextRol

-----------------------------------------------------------
-- SEARCH A ROL IN A CONTEXT AND ITS PROTOTYPES
-----------------------------------------------------------
-- | Search for a qualified ContextRol both in the local context and all its prototypes.
searchContextRol :: forall e. RolDef -> (AnyContext ~~> ContextRol) e
searchContextRol rn = searchLocallyAndInPrototypeHierarchy (context /-/ ((getContextRol rn) :: (AnyContext ~~> ContextRol) e))

-- | Search for a qualified ContextRol both in the local context and all its prototypes.
searchRolInContext :: forall e. RolDef -> (AnyContext ~~> RolInContext) e
searchRolInContext rn = searchLocallyAndInPrototypeHierarchy (context /-/ ((getRolInContext rn) :: (AnyContext ~~> RolInContext) e))

-- | Search for an unqualified rol both in the local context and all its prototypes.
-- TODO: hernoem getRolFromPrototypeHierarchy naar searchUnqualifiedRol
-- OF: let op of niet searchRolDefinitionInAspects gebruikt moet worden (mogelijke fout in aanroepende code!)
-- TODO: waarom alleen in ContextDef?
searchUnqualifiedRol :: forall e. Id.LocalName -> (AnyContext ~~> ContextRol) e
searchUnqualifiedRol rn = searchLocallyAndInPrototypeHierarchy (context /-/ ( (getUnqualifiedContextRol rn) :: (AnyContext ~~> ContextRol) e))

-- TODO: hernoem naar searchUnqualfiedContextRol en maak searchUnqualfiedRolinContext.

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
getUnqualifiedRolDefinition ::	forall e. Id.LocalName -> (ContextDef ~~> RolDef) e
getUnqualifiedRolDefinition ln = unwrap >>> (filter_
  (flip Id.hasLocalName ln)
  (getUnqualifiedContextRol "rolInContext" /-/ binding /-/ context))
    >=> (pure <<< map RolDef)

-- | Look for the definition of a Rol by its local name, in the ContextDef and its Aspects and in all their prototypes.
-- | As the name of a RolDefinition on an Aspect will be scoped to that Aspect, we do not have to search once
-- | we have the qualified name of the Rol. Hence there is no version that searches qualified roles!
-- TODO: replace uses of 'getRolUsingAspects' by this function.
-- getRolUsingAspects `psp:Rol -> ObjectsGetter`
searchUnqualifiedRolDefinition ::	forall e. Id.LocalName -> (ContextDef ~~> RolDef) e
searchUnqualifiedRolDefinition ln = unwrap >>> searchInAspectsAndPrototypes f
  where
    f :: (BuitenRol ~~> RolDef) e
    f = context >=> pure <<< map ContextDef /-/ getUnqualifiedRolDefinition ln

-----------------------------------------------------------
-- CHECK IF A CONTEXT DEFINITION HAS A ROL DEFINITION
-----------------------------------------------------------
hasLocalRolDefinition :: forall e. RolDef -> (ContextDef ~~> PBool) e
hasLocalRolDefinition qn = unwrap >>> contains (unwrap qn) (getUnqualifiedContextRol "rolInContext" /-/ binding /-/ context)

hasRolDefinition :: forall e. RolDef -> (ContextDef ~~> PBool) e
hasRolDefinition qn = unwrap >>> searchInAspectsAndPrototypes f
  where
    f :: (BuitenRol ~~> PBool) e
    f = context >=> pure <<< map ContextDef /-/ hasLocalRolDefinition qn
-----------------------------------------------------------
-- GET A PROPERTY FROM A ROLE TELESCOPE
-----------------------------------------------------------
-- | The value of the property pd, wherever in the telescope it is represented.
searchProperty :: forall b e. RolClass b => PropertyDef -> (b ~~> Value) e
searchProperty pd = typeWithPerspectivesTypes searchInRolTelescope g
  where
    g :: (b ~~> Value) e
    g = getProperty pd

-- | The value of the unqualified property pd, wherever in the telescope it is represented.
-- | NOTE: this function cannot be applied to a BinnenRol.
searchUnqualifiedProperty :: forall b e. RolClass b => Id.LocalName -> (b ~~> Value) e
searchUnqualifiedProperty pd = typeWithPerspectivesTypes searchInRolTelescope g
  where
    g :: (b ~~> Value) e
    g = getUnqualifiedProperty pd

-----------------------------------------------------------
-- GET A PROPERTY FROM A CONTEXT
-----------------------------------------------------------
-- | This is especially useful for the Binnen- and BuitenRol. E.g. it allows us to have a context with an external
-- | property that shadows the value of that property on the prototype of the context.

-- | Searches the qualified property first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
searchPropertyOnContext :: forall r e. RolClass r => (AnyContext ~~> r) e -> PropertyDef -> (AnyContext ~~> Value) e
searchPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (BuitenRol ~~> Value) e
    f = (context /-/ rolgetter /-/ ((searchProperty p)))

-- | Searches the property with the local name first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
searchUnqualifiedPropertyOnContext :: forall r e. RolClass r => (AnyContext ~~> r) e  -> Id.LocalName -> (AnyContext ~~> Value) e
searchUnqualifiedPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (BuitenRol ~~> Value) e
    f = (context /-/ rolgetter /-/ ((searchUnqualifiedProperty p)))

-- | Look for the property PropertyDef on the buitenRol of the ContextType c and on its telescope, shadowing any values
-- | on the prototypes.
searchExternalProperty :: forall e. PropertyDef -> (AnyContext ~~> Value) e
searchExternalProperty pn = searchPropertyOnContext buitenRol pn
-- searchExternalProperty pn = buitenRol /-/ searchProperty pn

-- | Look for the property with the given local name on the buitenRol of the ContextType c and on its telescope,
-- | shadowing any values on the prototypes.
searchExternalUnqualifiedProperty :: forall e. Id.LocalName -> (AnyContext ~~> Value) e
searchExternalUnqualifiedProperty ln = searchUnqualifiedPropertyOnContext buitenRol ln
-- searchExternalUnqualifiedProperty ln = buitenRol /-/ searchUnqualifiedProperty ln

-- | Look for the property with the given local name on the binnenRol of the ContextType c.
getInternalProperty :: forall e. PropertyDef -> (AnyContext ~~> Value) e
getInternalProperty pn = binnenRol /-/ getProperty pn

-- | Look for the property with the given local name on the binnenRol of the ContextType c and on its telescope.
searchInternalUnqualifiedProperty :: forall e. Id.LocalName -> (AnyContext ~~> Value) e
searchInternalUnqualifiedProperty ln = binnenRol /-/ searchUnqualifiedProperty ln

-- | From the instance of a Rol of any kind, find the instances of the Rol of the given type that bind it (that have
-- | it as their binding).
getGebondenAls :: forall r b e. RolClass r => RolClass b => RolDef -> (r ~~> b) e
getGebondenAls rname = typeWithPerspectivesTypes $ getRolMember \(PerspectRol{gevuldeRollen}) -> maybe [] id (lookup (unwrap rname) gevuldeRollen)

genericGetGebondenAls :: forall e. String -> (String ~~> String) e
genericGetGebondenAls rname = getRolMember \(PerspectRol{gevuldeRollen}) -> maybe [] id (lookup rname gevuldeRollen)

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
getUnqualifiedPropertyDefinition ::	forall e. Id.LocalName -> (RolDef ~~> PropertyDef) e
getUnqualifiedPropertyDefinition ln = unwrap >>> (filter_
  (flip Id.hasLocalName ln)
  (getUnqualifiedContextRol "rolInContext" /-/ binding /-/ context))
    >=> (pure <<< map PropertyDef)

-- | Look for the definition of a Property by its local name, in the RolDef and its Aspects and in all their prototypes.
searchUnqualifiedPropertyDefinition ::	forall e. Id.LocalName -> (RolDef ~~> PropertyDef) e
searchUnqualifiedPropertyDefinition ln = searchInMogelijkeBinding $ searchUnqualifiedPropertyDefinition' ln where

  searchUnqualifiedPropertyDefinition' ::	Id.LocalName -> (RolDef ~~> PropertyDef) e
  searchUnqualifiedPropertyDefinition' ln = unwrap >>> searchInAspectsAndPrototypes f
    where
      f :: (BuitenRol ~~> PropertyDef) e
      f = context >=> pure <<< map RolDef /-/ getUnqualifiedPropertyDefinition ln
