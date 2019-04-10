module Perspectives.ObjectGetterConstructors where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (cons, difference, elemIndex, foldMap, head, nub, null, union, singleton)
import Data.Array (filter, findIndex, index) as Arr
import Data.HeytingAlgebra (conj, disj, implies) as HA
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF, unwrap)
import Data.StrMap (keys, lookup)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_rolInContext)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (type (~~>), MP, ObjectsGetter)
import Perspectives.DataTypeObjectGetters (binnenRol, buitenRol, context, rolBindingDef)
import Perspectives.Identifiers (LocalName, hasLocalName) as Id
import Perspectives.ObjectsGetterComposition (composeMonoidal, (/-/), (\-\))
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, BuitenRol, ContextDef(..), ContextRol, PBool(..), PropertyDef(..), RolDef(..), RolInContext, Value, AnyDefinition, binding, genericBinding, getProperty, getUnqualifiedProperty, typeWithPerspectivesTypes)
import Perspectives.Syntax (PerspectContext, PerspectRol(..))
import Prelude (class Eq, bind, const, flip, id, join, map, pure, show, ($), (<<<), (<>), (==), (>=>), (>>=), (>>>))

-- | This module only exports constructors that search roles or properties,
-- | in the space of prototypes, Aspects, AspectRoles and AspectProperties.

-----------------------------------------------------------
-- COMBINATORS
-----------------------------------------------------------
-- | The closure of an ObjectsGetter (cumulates results, excluding the root).
-- Test.Perspectives.ObjectGetterConstructors, via closureOfBinding.
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
        (results :: Array (Array o)) <- traverse (getter (cons id cumulator)) (difference objectsOfP cumulator)
        pure $ nub $ join (cons objectsOfP results)
      otherwise -> pure objectsOfP

-- | The closure of an ObjectsGetter including the root.

-- Test.Perspectives.ObjectGetterConstructors
closure_ :: forall o e.
  Eq o =>
  (o ~~> o) e ->
  (o ~~> o) e
closure_ p id = closure p id >>= pure <<< cons id

-- | Combinator to make an ObjectsGetter fail if it returns an empty result.
-- | Useful in combination with computing alternatives using <|>
unlessNull :: forall s o e. (s ~~> o) e -> (s ~~> o) e
unlessNull og id = og id >>= \r -> if (null r) then empty else pure r

-- | Combinator to make an ObjectsGetter fail if it returns PBool "false".
-- | Useful in combination with computing alternatives using <|>
unlessFalse :: forall s e. (s ~~> PBool) e -> (s ~~> PBool) e
unlessFalse og id = og id >>= \r -> case (elemIndex (PBool "false") r) of
  Nothing -> pure r
  otherwise -> empty

-- Test.Perspectives.ObjectGetterConstructors
contains :: forall s o e. Eq o => o -> (s ~~> o) e -> (s ~~> PBool) e
contains o getter = getter >=> \(os :: Array o) -> case elemIndex o os of
  Nothing -> pure [PBool "false"]
  otherwise -> pure [PBool "true"]

-- Test.Perspectives.ObjectGetterConstructors, via getUnqualifiedRolDefinition
filter_ :: forall s o e.
  (o -> Boolean) ->
  (s ~~> o) e ->
  (s ~~> o) e
filter_ criterium getter = getter >=> pure <<< Arr.filter criterium

toBoolean :: forall s e. (s ~~> PBool) e -> s -> MP e Boolean
toBoolean g = g >=> \(bs :: Array PBool) -> case head bs of
  Nothing -> pure true
  (Just b) -> pure (b == PBool "true")

cond :: forall s o e.
  (s ~~> PBool) e ->
  (s ~~> o) e ->
  (s ~~> o) e ->
  (s ~~> o) e
cond condition thenPart elsePart id = do
  cs <- condition id
  case head cs of
    Just (PBool "true") -> thenPart id
    otherwise -> elsePart id

-- | Applies the logical binary operator (such as OR, AND and IMPLIES) to the results of two queries applied to the same origin.
-- | Note that just the first results of the argument ObjectGetters are used!
logicalBinaryOperator :: forall s e.
  (Boolean -> Boolean -> Boolean) ->
  (s ~~> PBool) e ->
  (s ~~> PBool) e ->
  (s ~~> PBool) e
logicalBinaryOperator op p q id = do
  ps <- p id
  qs <- q id
  pure $ fromBool $ op (toBool ps) (toBool qs)
  where
    fromBool :: Boolean -> Array PBool
    fromBool = singleton <<< PBool <<< show

    toBool :: Array PBool -> Boolean
    toBool s = maybe false ((==) (PBool "true")) (head s)

conj :: forall s e. (s ~~> PBool) e -> (s ~~> PBool) e -> ((s ~~> PBool) e)
conj = logicalBinaryOperator HA.conj

disj :: forall s e. (s ~~> PBool) e -> (s ~~> PBool) e -> ((s ~~> PBool) e)
disj = logicalBinaryOperator HA.disj

implies :: forall s e. (s ~~> PBool) e -> (s ~~> PBool) e -> ((s ~~> PBool) e)
implies = logicalBinaryOperator HA.implies

notEmpty :: forall s o e. (s ~~> o) e -> (s ~~> PBool) e
notEmpty p = p >=> \os -> pure $ [PBool $ show $ not (null os)]

-- Test.Perspectives.ObjectGetterConstructors, via searchProperty
searchInRolTelescope :: forall e. ObjectsGetter e -> ObjectsGetter e
searchInRolTelescope getter rolId =
  unlessNull getter rolId
  <|>
  (genericBinding /-/ searchInRolTelescope getter) rolId

-- | Applies the ObjectsGetter to each higher prototype until it succeeds or there is no prototype.
-- | Does *not* apply the getter to the ContextType that is passed in!
-- Test.Perspectives.ObjectGetterConstructors
searchInPrototypeHierarchy :: forall o e.
  Eq o =>
  (BuitenRol ~~> o) e ->
  (AnyContext ~~> o) e
searchInPrototypeHierarchy getter = buitenRol /-/ typeWithPerspectivesTypes searchInRolTelescope getter

-- | Applies the getter (AnyContext ~~> o) e to any context and all its prototypes.
-- Test.Perspectives.ObjectGetterConstructors, via buitenRolBeschrijvingDef
searchLocallyAndInPrototypeHierarchy :: forall o e.
  Eq o =>
  (AnyContext ~~> o) e ->
  (AnyContext ~~> o) e
searchLocallyAndInPrototypeHierarchy getter c =
  unlessNull getter c
  <|>
  (buitenRol /-/ binding /-/ context /-/ searchLocallyAndInPrototypeHierarchy getter) c

-- | Applies the getter (s ~~> o) e to the ContextType and all its prototypes and recursively to all its aspects.
-- Test.Perspectives.ObjectGetterConstructors, via buitenRolBeschrijvingDef
searchInAspectsAndPrototypes :: forall o e.
  Eq o =>
  (AnyContext ~~> o) e ->
  (AnyContext ~~> o) e
searchInAspectsAndPrototypes getter contextId =
  unlessNull (searchLocallyAndInPrototypeHierarchy getter) contextId
  <|>
  (directAspects /-/ searchInAspectsAndPrototypes getter) contextId

checkInAspectsAndPrototypes :: forall e.
  (AnyContext ~~> PBool) e ->
  (AnyContext ~~> PBool) e
-- Test.Perspectives.ObjectGetterConstructors, via hasRolDefinition
checkInAspectsAndPrototypes getter contextId =
  unlessFalse (searchLocallyAndInPrototypeHierarchy getter) contextId
  <|>
  (directAspects /-/ checkInAspectsAndPrototypes getter) contextId

-- | Applies the getter (s ~~> o) e to the RolDef and all its prototypes and recursively to all its aspects.
-- Test.Perspectives.ObjectGetterConstructors via searchUnqualifiedPropertyDefinition.
searchInAspectRolesAndPrototypes :: forall o e.
  Eq o =>
  (AnyContext ~~> o) e ->
  (AnyContext ~~> o) e
searchInAspectRolesAndPrototypes getter contextId =
  unlessNull (searchLocallyAndInPrototypeHierarchy getter) contextId
  <|>
  (directAspectRoles /-/ (unwrap >>> searchInAspectRolesAndPrototypes getter)) (RolDef contextId)

-- Test.Perspectives.ObjectGetterConstructors
directAspects :: forall e. (AnyContext ~~> AnyContext) e
directAspects = getContextRol (RolDef "model:Perspectives$Context$aspect") /-/ rolBindingDef

-- Test.Perspectives.ObjectGetterConstructors
directAspectRoles :: forall e. (RolDef ~~> RolDef) e
directAspectRoles = typeWithPerspectivesTypes $ getContextRol (RolDef "model:Perspectives$Rol$aspectRol") /-/ rolBindingDef

directAspectProperties :: forall e. (PropertyDef ~~> PropertyDef) e
directAspectProperties = typeWithPerspectivesTypes $ getContextRol (RolDef "model:Perspectives$Property$aspectProperty") /-/ rolBindingDef

-- | The type of Rol or Context that can be bound to the Rol.
-- Test.Perspectives.ObjectGetterConstructors
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

agreesWithType :: forall e. AnyDefinition -> (AnyDefinition ~~> PBool) e
agreesWithType t = if t == "model:Perspectives$ElkType"
  then const (pure [PBool "true"])
  else if t == "model:Perspectives$Niets"
    then const (pure [PBool "false"])
    else \x -> if (x == "model:Perspectives$ElkType")
        then pure [PBool "true"]
        else if (x == "model:Perspectives$Niets")
          then pure [PBool "false"]
          else if (t == x)
            then pure $ [PBool "true"]
            else pure $ [PBool "false"]

-- Test.Perspectives.ObjectGetterConstructors
concat :: forall s o e. Eq o => (s ~~> o) e -> (s ~~> o) e -> (s ~~> o) e
concat f p s = do
  fs <- f s
  ps <- p s
  pure $ union fs ps

-- | True iff at least one of the boolean results of f is true (where true is represented as PBool "true").
-- Test.Perspectives.ObjectGetterConstructors
some :: forall s e. (s ~~> PBool) e -> (s ~~> PBool) e
some f = f `composeMonoidal` (alaF Disj foldMap ((==) (PBool "true")) >>> show >>> PBool)

-- | True iff at all of the boolean results of f is true (where true is represented as PBool "true").
-- Test.Perspectives.ObjectGetterConstructors
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
-- Test.Perspectives.ObjectGetterConstructors via getRolinContext
getContextRol :: forall e. RolDef -> (AnyContext ~~> ContextRol) e
getContextRol rn = typeWithPerspectivesTypes $ getContextMember \context -> maybe [] id (lookup (unwrap rn) (context_rolInContext context))

-- | As getContextRol, but for RolinContext (same function, differently typed).
-- Test.Perspectives.ObjectGetterConstructors
getRolInContext :: forall e. RolDef -> (AnyContext ~~> RolInContext) e
getRolInContext = typeWithPerspectivesTypes getContextRol

-- | Get the ContextRol instances with the given local name directly from the Context.
-- E.g. getUnqualifiedContextRol "rolProperty" will return the same result as getContextRol
-- "model:Perspectives$View$rolProperty".
-- Test.Perspectives.ObjectGetterConstructors via getUnqualifiedRolInContext
getUnqualifiedContextRol :: forall e. Id.LocalName -> (AnyContext ~~> ContextRol) e
getUnqualifiedContextRol ln = typeWithPerspectivesTypes (getContextMember $ getUnQualifiedRolFromPerspectContext ln)
  where
  getUnQualifiedRolFromPerspectContext :: Id.LocalName -> PerspectContext -> Array String
  getUnQualifiedRolFromPerspectContext ln ctxt =
    case Arr.findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys $ context_rolInContext ctxt) of
      Nothing -> []
      (Just i) -> maybe [] id (lookup (unsafePartial $ fromJust (Arr.index (keys $ context_rolInContext ctxt) i)) (context_rolInContext ctxt))

-- | As getUnqualifiedContextRol, but for RolinContext (same function, differently typed).
-- Test.Perspectives.ObjectGetterConstructors
getUnqualifiedRolInContext :: forall e. Id.LocalName -> (AnyContext ~~> RolInContext) e
getUnqualifiedRolInContext = typeWithPerspectivesTypes getUnqualifiedContextRol

-----------------------------------------------------------
-- SEARCH A ROL IN A CONTEXT AND ITS PROTOTYPES
-----------------------------------------------------------
-- | Search for a qualified ContextRol both in the local context and all its prototypes.
-- searchLocallyAndInPrototypeHierarchy and getContextRol are tested.
-- Test.Perspectives.ObjectGetterConstructors
searchContextRol :: forall e. RolDef -> (AnyContext ~~> ContextRol) e
searchContextRol rn = searchLocallyAndInPrototypeHierarchy ((getContextRol rn) :: (AnyContext ~~> ContextRol) e)

-- | Search for a qualified ContextRol both in the local context and all its prototypes.
-- searchLocallyAndInPrototypeHierarchy and getContextRol are tested.
searchRolInContext :: forall e. RolDef -> (AnyContext ~~> RolInContext) e
searchRolInContext rn = searchLocallyAndInPrototypeHierarchy ((getRolInContext rn) :: (AnyContext ~~> RolInContext) e)

-- | Search for an unqualified rol both in the local context and all its prototypes.
-- Test.Perspectives.ObjectGetterConstructors
searchUnqualifiedRol :: forall e. Id.LocalName -> (AnyContext ~~> ContextRol) e
searchUnqualifiedRol rn = searchLocallyAndInPrototypeHierarchy ( (getUnqualifiedContextRol rn) :: (AnyContext ~~> ContextRol) e)

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
-- Test.Perspectives.ObjectGetterConstructors
getUnqualifiedRolDefinition ::	forall e. Id.LocalName -> (ContextDef ~~> RolDef) e
getUnqualifiedRolDefinition ln = unwrap >>> (filter_
  (flip Id.hasLocalName ln)
  (getUnqualifiedContextRol "rolInContext" /-/ binding /-/ context))
    >=> (pure <<< map RolDef)

-- | Look for the definition of a Rol by its local name, in the ContextDef and its Aspects and in all their prototypes.
-- | As the name of a RolDefinition on an Aspect will be scoped to that Aspect, we do not have to search once
-- | we have the qualified name of the Rol. Hence there is no version that searches qualified roles!
-- Test.Perspectives.ObjectGetterConstructors
searchUnqualifiedRolDefinition ::	forall e. Id.LocalName -> (ContextDef ~~> RolDef) e
searchUnqualifiedRolDefinition ln = unwrap >>> searchInAspectsAndPrototypes f
  where
    f :: (AnyContext ~~> RolDef) e
    f = pure <<< ContextDef >=> getUnqualifiedRolDefinition ln

-----------------------------------------------------------
-- CHECK IF A CONTEXT DEFINITION HAS A ROL DEFINITION
-----------------------------------------------------------
-- Test.Perspectives.ObjectGetterConstructors
hasLocalRolDefinition :: forall e. RolDef -> (ContextDef ~~> PBool) e
hasLocalRolDefinition qn = unwrap >>> contains (unwrap qn) (getUnqualifiedContextRol "rolInContext" /-/ binding /-/ context)

-- Test.Perspectives.ObjectGetterConstructors
hasRolDefinition :: forall e. RolDef -> (ContextDef ~~> PBool) e
hasRolDefinition qn = unwrap >>> checkInAspectsAndPrototypes f
  where
    f :: (AnyContext ~~> PBool) e
    f = pure <<< ContextDef >=> hasLocalRolDefinition qn
-----------------------------------------------------------
-- GET A PROPERTY FROM A ROLE TELESCOPE
-----------------------------------------------------------
-- | The value of the property pd, wherever in the telescope it is represented.
-- Test.Perspectives.ObjectGetterConstructors
searchProperty :: forall b e. RolClass b => PropertyDef -> (b ~~> Value) e
searchProperty pd = typeWithPerspectivesTypes searchInRolTelescope g
  where
    g :: (b ~~> Value) e
    g = getProperty pd

-- | The value of the unqualified property pd, wherever in the telescope it is represented.
-- | NOTE: this function cannot be applied to a BinnenRol.
-- Test.Perspectives.ObjectGetterConstructors
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
-- Test.Perspectives.ObjectGetterConstructors via searchExternalProperty
searchPropertyOnContext :: forall r e. RolClass r => (AnyContext ~~> r) e -> PropertyDef -> (AnyContext ~~> Value) e
searchPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (AnyContext ~~> Value) e
    f = (rolgetter /-/ ((searchProperty p)))

-- | Searches the property with the local name first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
-- Test.Perspectives.ObjectGetterConstructors via searchExternalUnqualifiedProperty
searchUnqualifiedPropertyOnContext :: forall r e. RolClass r => (AnyContext ~~> r) e  -> Id.LocalName -> (AnyContext ~~> Value) e
searchUnqualifiedPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (AnyContext ~~> Value) e
    f = (rolgetter /-/ ((searchUnqualifiedProperty p)))

-- | Look for the property PropertyDef on the buitenRol of the ContextType c and on its telescope, shadowing any values
-- | on the prototypes. This function is cumbersome to use, because the full name of an external
-- | property includes 'buitenRolBeschrijving'.
-- Test.Perspectives.ObjectGetterConstructors
searchExternalProperty :: forall e. PropertyDef -> (AnyContext ~~> Value) e
searchExternalProperty pn = searchPropertyOnContext buitenRol pn
-- searchExternalProperty pn = buitenRol /-/ searchProperty pn

-- | Look for the property with the given local name on the buitenRol of the ContextType c and on its telescope,
-- | shadowing any values on the prototypes.
-- Test.Perspectives.ObjectGetterConstructors
searchExternalUnqualifiedProperty :: forall e. Id.LocalName -> (AnyContext ~~> Value) e
searchExternalUnqualifiedProperty ln = searchUnqualifiedPropertyOnContext buitenRol ln

-- | Look for the property with the given qualified name on the binnenRol of the ContextType c.
-- Test.Perspectives.ObjectGetterConstructors
getInternalProperty :: forall e. PropertyDef -> (AnyContext ~~> Value) e
getInternalProperty pn = binnenRol /-/ getProperty pn

-- Test.Perspectives.ObjectGetterConstructors via searchInternalUnqualifiedProperty
getInternalUnqualifiedProperty :: forall e. Id.LocalName -> (AnyContext ~~> Value) e
getInternalUnqualifiedProperty ln = binnenRol /-/ getUnqualifiedProperty ln

-- | Look for the property with the given local name on the binnenRol of the ContextType c and on its telescope.
-- Test.Perspectives.ObjectGetterConstructors
searchInternalUnqualifiedProperty :: forall e. Id.LocalName -> (AnyContext ~~> Value) e
searchInternalUnqualifiedProperty ln cid = unlessNull (getInternalUnqualifiedProperty ln) cid
  <|>
  (searchExternalUnqualifiedProperty ln cid)

-- | From the instance of a Rol of any kind, find the instances of the Rol of the given type that bind it (that have
-- | it as their binding). The type of rname (RolDef) can be a BuitenRol.
-- Test.Perspectives.ObjectGetterConstructors
getRoleBinders :: forall r b e. RolClass r => RolClass b => RolDef -> (r ~~> b) e
getRoleBinders rname = typeWithPerspectivesTypes $ getRolMember \(PerspectRol{gevuldeRollen}) -> maybe [] id (lookup (unwrap rname) gevuldeRollen)

-- | From the instance of a Rol of any kind, find the instances of the Rol with the given local name
-- | that bind it (that have it as their binding). The type of ln can be 'buitenRolBeschrijving'.
-- Test.Perspectives.ObjectGetterConstructors
getUnqualifiedRoleBinders :: forall r b e. RolClass r => RolClass b => Id.LocalName -> (r ~~> b) e
getUnqualifiedRoleBinders ln = typeWithPerspectivesTypes $ getRolMember \(PerspectRol{gevuldeRollen}) ->
    case Arr.findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys gevuldeRollen) of
      Nothing -> []
      (Just i) -> maybe [] id (lookup (unsafePartial $ fromJust (Arr.index (keys gevuldeRollen) i)) gevuldeRollen)

genericGetRoleBinders :: forall e. String -> (String ~~> String) e
genericGetRoleBinders rname = getRolMember \(PerspectRol{gevuldeRollen}) -> maybe [] id (lookup rname gevuldeRollen)

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
-- Test.Perspectives.ObjectGetterConstructors
getUnqualifiedPropertyDefinition ::	forall e. Id.LocalName -> (RolDef ~~> PropertyDef) e
getUnqualifiedPropertyDefinition ln = unwrap >>> (filter_
  (flip Id.hasLocalName ln)
  (getUnqualifiedContextRol "rolProperty" /-/ binding /-/ context))
    >=> (pure <<< map PropertyDef)

-- | Look for the definition of a Property by its local name, in the RolDef and its AspectRoles and in all their prototypes.
-- Test.Perspectives.ObjectGetterConstructors
searchUnqualifiedPropertyDefinition ::	forall e. Id.LocalName -> (RolDef ~~> PropertyDef) e
searchUnqualifiedPropertyDefinition ln = searchUnqualifiedPropertyDefinition' ln where

  searchUnqualifiedPropertyDefinition' ::	Id.LocalName -> (RolDef ~~> PropertyDef) e
  searchUnqualifiedPropertyDefinition' ln = unwrap >>> searchInAspectRolesAndPrototypes f
    where
      f :: (AnyContext ~~> PropertyDef) e
      f = pure <<< RolDef >=> getUnqualifiedPropertyDefinition ln
