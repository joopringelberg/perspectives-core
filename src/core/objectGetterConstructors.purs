module Perspectives.ObjectGetterConstructors where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (cons, difference, elemIndex, foldM, foldMap, head, nub, null, singleton, union)
import Data.Array (filter, findIndex, index) as Arr
import Data.HeytingAlgebra (conj, disj, implies) as HA
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Monoid.Conj (Conj(..))
import Data.Monoid.Disj (Disj(..))
import Data.Newtype (alaF, unwrap)
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Foreign.Object (keys, lookup)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_iedereRolInContext)
import Perspectives.ContextRolAccessors (getContextMember, getRolMember)
import Perspectives.CoreTypes (type (~~>), MP)
import Perspectives.InstanceRepresentation (PerspectContext, PerspectRol(..))
import Perspectives.Instances.Aliases (LocalName)
import Perspectives.Instances.ObjectGetters (binding, buitenRol, context, getProperty, getUnqualifiedProperty)
import Perspectives.ObjectsGetterComposition (composeMonoidal, (/-/))
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance, Value)
import Perspectives.Representation.TypeIdentifiers (EnumeratedPropertyType, EnumeratedRoleType)
import Prelude (class Eq, class Ord, bind, flip, identity, join, pure, show, ($), (<<<), (<>), (==), (>=>), (>>=), (>>>))

-- | This module only exports constructors that search roles or properties,
-- | in the space of prototypes, Aspects, AspectRoles and AspectProperties.

-----------------------------------------------------------
-- COMBINATORS
-----------------------------------------------------------
-- | The closure of an ObjectsGetter (cumulates results, excluding the root).
-- Test.Perspectives.ObjectGetterConstructors, via closureOfBinding.
closure :: forall o.
  Eq o =>
  Ord o =>
  (o ~~> o) ->
  (o ~~> o)
closure p = getter [] where
  getter :: Array o -> o -> MP (Array o)
  getter cumulator id = do
    (objectsOfP :: Array o) <- p id
    case elemIndex id cumulator of
      Nothing -> do
        (results :: Array (Array o)) <- traverse (getter (cons id cumulator)) (difference objectsOfP cumulator)
        pure $ nub $ join (cons objectsOfP results)
      otherwise -> pure objectsOfP

-- | The closure of an ObjectsGetter including the root.

-- Test.Perspectives.ObjectGetterConstructors
closure_ :: forall o.
  Eq o =>
  Ord o =>
  (o ~~> o) ->
  (o ~~> o)
closure_ p id = closure p id >>= pure <<< cons id

-- | Combinator to make an ObjectsGetter fail if it returns an empty result.
-- | Useful in combination with computing alternatives using <|>
unlessNull :: forall s o. (s ~~> o) -> (s ~~> o)
unlessNull og id = og id >>= \r -> if (null r) then empty else pure r

-- | Combinator to make an ObjectsGetter fail if it returns "false".
-- | Useful in combination with computing alternatives using <|>
unlessFalse :: forall s. (s ~~> String) -> (s ~~> String)
unlessFalse og id = og id >>= \r -> case (elemIndex ("false") r) of
  Nothing -> pure r
  otherwise -> empty

-- Test.Perspectives.ObjectGetterConstructors
containedIn :: forall s o. Eq o => o -> (s ~~> o) -> (s ~~> String)
containedIn o getter = getter >=> \(os :: Array o) -> case elemIndex o os of
  Nothing -> pure ["false"]
  otherwise -> pure ["true"]

contains :: forall s o. Eq o => (s ~~> o) -> o -> (s ~~> String)
contains = flip containedIn

-- | A selection of the results of the second query using the first (boolean) query as a criterium.
-- | `psp:Constraint -> psp:Function -> psp:Function`
filter :: forall s o.
  (o ~~> String) ->
  (s ~~> o) ->
  (s ~~> o)
filter criterium p id = do
  objects <- p id
  foldM (\cum obj -> toBoolean criterium obj >>= (\include -> if include then pure $ cons obj cum else pure cum)) [] objects


-- Test.Perspectives.ObjectGetterConstructors, via getUnqualifiedRolDefinition
filter_ :: forall s o.
  (o -> Boolean) ->
  (s ~~> o) ->
  (s ~~> o)
filter_ criterium getter = getter >=> pure <<< Arr.filter criterium

toBoolean :: forall s. (s ~~> String) -> s -> MP Boolean
toBoolean g = g >=> \(bs :: Array String) -> case head bs of
  Nothing -> pure true
  (Just b) -> pure (b == "true")

cond :: forall s o.
  (s ~~> String) ->
  (s ~~> o) ->
  (s ~~> o) ->
  (s ~~> o)
cond condition thenPart elsePart id = do
  cs <- condition id
  case head cs of
    Just ("true") -> thenPart id
    otherwise -> elsePart id

-- | Applies the logical binary operator (such as OR, AND and IMPLIES) to the results of two queries applied to the same origin.
-- | Note that just the first results of the argument ObjectGetters are used!
logicalBinaryOperator :: forall s.
  (Boolean -> Boolean -> Boolean) ->
  (s ~~> String) ->
  (s ~~> String) ->
  (s ~~> String)
logicalBinaryOperator op p q id = do
  ps <- p id
  qs <- q id
  pure $ fromBool $ op (toBool ps) (toBool qs)
  where
    fromBool :: Boolean -> Array String
    fromBool = singleton <<< show

    toBool :: Array String -> Boolean
    toBool s = maybe false ((==) ("true")) (head s)

conj :: forall s. (s ~~> String) -> (s ~~> String) -> ((s ~~> String))
conj = logicalBinaryOperator HA.conj

disj :: forall s. (s ~~> String) -> (s ~~> String) -> ((s ~~> String))
disj = logicalBinaryOperator HA.disj

implies :: forall s. (s ~~> String) -> (s ~~> String) -> ((s ~~> String))
implies = logicalBinaryOperator HA.implies

notEmpty :: forall s o. (s ~~> o) -> (s ~~> String)
notEmpty p = p >=> \os -> pure $ [show $ not (null os)]

-- Test.Perspectives.ObjectGetterConstructors, via searchProperty
searchInRolTelescope :: forall a. Eq a => (RoleInstance ~~> a) -> (RoleInstance ~~> a)
searchInRolTelescope getter rolId =
  unlessNull getter rolId
  <|>
  (binding /-/ (searchInRolTelescope getter)) rolId

-- | Applies the ObjectsGetter to each higher prototype until it succeeds or there is no prototype.
-- | Does *not* apply the getter to the ContextType that is passed in!
-- Test.Perspectives.ObjectGetterConstructors
searchInPrototypeHierarchy ::
  (RoleInstance ~~> Value) ->
  (ContextInstance ~~> Value)
searchInPrototypeHierarchy getter = buitenRol /-/  searchInRolTelescope getter

-- | Applies the getter (AnyContext ~~> o) to any context and all its prototypes.
-- Test.Perspectives.ObjectGetterConstructors, via buitenRolBeschrijvingDef
searchLocallyAndInPrototypeHierarchy :: forall o.
  Eq o =>
  (ContextInstance ~~> o) ->
  (ContextInstance ~~> o)
searchLocallyAndInPrototypeHierarchy getter c =
  unlessNull getter c
  <|>
  (buitenRol /-/ binding /-/ context /-/ searchLocallyAndInPrototypeHierarchy getter) c

-- Test.Perspectives.ObjectGetterConstructors
concat :: forall s o. Eq o => (s ~~> o) -> (s ~~> o) -> (s ~~> o)
concat f p s = do
  fs <- f s
  ps <- p s
  pure $ union fs ps

-- | True iff at least one of the boolean results of f is true (where true is represented as "true").
-- Test.Perspectives.ObjectGetterConstructors
some :: forall s. (s ~~> String) -> (s ~~> String)
some f = f `composeMonoidal` (alaF Disj foldMap ((==) ("true")) >>> show)

-- | True iff at all of the boolean results of f is true (where true is represented as "true").
-- Test.Perspectives.ObjectGetterConstructors
all :: forall s. (s ~~> String) -> (s ~~> String)
all f = f `composeMonoidal` (alaF Conj foldMap ((==) ("true")) >>> show)

-----------------------------------------------------------
-- CLOSURES
-----------------------------------------------------------
-- | All role instances in the telescope, excluding the root.
-- | A closure must be homogeneously typed. Here we require that the members of the collection are instances
-- | of the class Binding.
closureOfBinding :: RoleInstance ~~> RoleInstance
closureOfBinding = closure binding

-- | The prototype of a ContextType.
getPrototype :: (ContextInstance ~~> ContextInstance)
getPrototype = (buitenRol /-/ binding /-/ context)

-- | All prototypes of a ContextType, excluding the ContextType itself. A homogeneous collection of AnyDefinition.
closureOfPrototype :: ContextInstance ~~> ContextInstance
closureOfPrototype = closure getPrototype

-----------------------------------------------------------
-- GET A ROL FROM A CONTEXT
-----------------------------------------------------------
-- | Get the ContextRol instances with the given rol name (RolDef) directly from the Context definition (not searching prototypes or Aspects).
-- | E.g. getRol "model:Perspectives$View$rolProperty" will return all rol instances that bind a PropertyDef on an instance of psp:View.
-- Test.Perspectives.ObjectGetterConstructors via getRolinContext
getRol :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
getRol rn = getContextMember \context -> maybe [] identity (lookup (unwrap rn) (context_iedereRolInContext context))

-- | Get the ContextRol instances with the given local name directly from the Context.
-- E.g. getUnqualifiedContextRol "rolProperty" will return the same result as getRol
-- "model:Perspectives$View$rolProperty".
-- Test.Perspectives.ObjectGetterConstructors via getUnqualifiedRolInContext
getUnqualifiedContextRol :: LocalName -> (ContextInstance ~~> RoleInstance)
getUnqualifiedContextRol ln' =  (getContextMember $ getUnQualifiedRolFromPerspectContext ln')
  where
  getUnQualifiedRolFromPerspectContext :: LocalName -> PerspectContext -> Array RoleInstance
  getUnQualifiedRolFromPerspectContext ln ctxt =
    case Arr.findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys $ context_iedereRolInContext ctxt) of
      Nothing -> []
      (Just i) -> maybe [] identity (lookup (unsafePartial $ fromJust (Arr.index (keys $ context_iedereRolInContext ctxt) i)) (context_iedereRolInContext ctxt))

-- | As getUnqualifiedContextRol, but for RolinContext (same function, differently typed).
-- Test.Perspectives.ObjectGetterConstructors
getUnqualifiedRolInContext :: LocalName -> (ContextInstance ~~> RoleInstance)
getUnqualifiedRolInContext =  getUnqualifiedContextRol

-----------------------------------------------------------
-- SEARCH A ROL IN A CONTEXT AND ITS PROTOTYPES
-----------------------------------------------------------
-- | Search for a qualified ContextRol both in the local context and all its prototypes.
-- searchLocallyAndInPrototypeHierarchy and getRol are tested.
-- Test.Perspectives.ObjectGetterConstructors
searchContextRol :: EnumeratedRoleType -> (ContextInstance ~~> RoleInstance)
searchContextRol rn = searchLocallyAndInPrototypeHierarchy ((getRol rn) :: (ContextInstance ~~> RoleInstance))

-- | Search for a qualified ContextRol both in the local context and all its prototypes.
-- searchLocallyAndInPrototypeHierarchy and getRol are tested.
searchRolInContext :: EnumeratedRoleType-> (ContextInstance ~~> RoleInstance)
searchRolInContext rn = searchLocallyAndInPrototypeHierarchy ((getRol rn) :: (ContextInstance ~~> RoleInstance))

-- | Search for an unqualified rol both in the local context and all its prototypes.
-- Test.Perspectives.ObjectGetterConstructors
searchUnqualifiedRol :: LocalName -> (ContextInstance ~~> RoleInstance)
searchUnqualifiedRol rn = searchLocallyAndInPrototypeHierarchy ( (getUnqualifiedContextRol rn) :: (ContextInstance ~~> RoleInstance))

-- TODO: hernoem naar searchUnqualfiedContextRol en maak searchUnqualfiedRolinContext.

-----------------------------------------------------------
-- GET A PROPERTY FROM A ROLE TELESCOPE
-----------------------------------------------------------
-- | The value of the property pd, wherever in the telescope it is represented.
-- Test.Perspectives.ObjectGetterConstructors
searchProperty :: EnumeratedPropertyType -> (RoleInstance ~~> Value)
searchProperty pd =  searchInRolTelescope g
  where
    g :: (RoleInstance ~~> Value)
    g = getProperty pd

-- | The value of the unqualified property pd, wherever in the telescope it is represented.
-- | NOTE: this function cannot be applied to a BinnenRol.
-- Test.Perspectives.ObjectGetterConstructors
searchUnqualifiedProperty :: LocalName -> (RoleInstance ~~> Value)
searchUnqualifiedProperty pd =  searchInRolTelescope g
  where
    g :: (RoleInstance ~~> Value)
    g = getUnqualifiedProperty pd

-----------------------------------------------------------
-- GET A PROPERTY FROM A CONTEXT
-----------------------------------------------------------
-- | This is especially useful for the Binnen- and BuitenRol. E.g. it allows us to have a context with an external
-- | property that shadows the value of that property on the prototype of the context.

-- | Searches the qualified property first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
-- Test.Perspectives.ObjectGetterConstructors via searchExternalProperty
searchPropertyOnContext :: (ContextInstance ~~> RoleInstance) -> EnumeratedPropertyType -> (ContextInstance ~~> Value)
searchPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (ContextInstance ~~> Value)
    f = (rolgetter /-/ ((searchProperty p)))

-- | Searches the property with the local name first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
-- Test.Perspectives.ObjectGetterConstructors via searchExternalUnqualifiedProperty
searchUnqualifiedPropertyOnContext :: (ContextInstance ~~> RoleInstance)  -> LocalName -> (ContextInstance ~~> Value)
searchUnqualifiedPropertyOnContext rolgetter p = searchLocallyAndInPrototypeHierarchy f
  where
    f :: (ContextInstance ~~> Value)
    f = (rolgetter /-/ ((searchUnqualifiedProperty p)))

-- | Look for the property PropertyDef on the buitenRol of the ContextType c and on its telescope, shadowing any values
-- | on the prototypes. This function is cumbersome to use, because the full name of an external
-- | property includes 'buitenRolBeschrijving'.
-- Test.Perspectives.ObjectGetterConstructors
searchExternalProperty :: EnumeratedPropertyType -> (ContextInstance ~~> Value)
searchExternalProperty pn = searchPropertyOnContext buitenRol pn
-- searchExternalProperty pn = buitenRol /-/ searchProperty pn

-- | Look for the property with the given local name on the buitenRol of the ContextType c and on its telescope,
-- | shadowing any values on the prototypes.
-- Test.Perspectives.ObjectGetterConstructors
searchExternalUnqualifiedProperty :: LocalName -> (ContextInstance ~~> Value)
searchExternalUnqualifiedProperty ln = searchUnqualifiedPropertyOnContext buitenRol ln

-- | From the instance of a Rol of any kind, find the instances of the Rol of the given type that bind it (that have
-- | it as their binding). The type of rname (RolDef) can be a BuitenRol.
-- Test.Perspectives.ObjectGetterConstructors
getRoleBinders :: EnumeratedRoleType -> (RoleInstance ~~> RoleInstance)
getRoleBinders rname = getRolMember \(PerspectRol{gevuldeRollen}) -> maybe [] identity (lookup (unwrap rname) gevuldeRollen)

-- | From the instance of a Rol of any kind, find the instances of the Rol with the given local name
-- | that bind it (that have it as their binding). The type of ln can be 'buitenRolBeschrijving'.
-- Test.Perspectives.ObjectGetterConstructors
getUnqualifiedRoleBinders :: LocalName -> (RoleInstance ~~> RoleInstance)
getUnqualifiedRoleBinders ln = getRolMember \(PerspectRol{gevuldeRollen}) ->
    case Arr.findIndex (test (unsafeRegex (ln <> "$") noFlags)) (keys gevuldeRollen) of
      Nothing -> []
      (Just i) -> maybe [] identity (lookup (unsafePartial $ fromJust (Arr.index (keys gevuldeRollen) i)) gevuldeRollen)
