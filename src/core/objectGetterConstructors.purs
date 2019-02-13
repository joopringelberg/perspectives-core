module Perspectives.ObjectGetterConstructors where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (null, elemIndex, union, difference, nub, cons, foldMap)
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
import Perspectives.Identifiers (LocalName) as Id
import Perspectives.ObjectsGetterComposition (composeMonoidal, (/-/))
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, BuitenRol, Context(..), ContextDef(..), ContextRol, PBool(..), PropertyDef, RolDef(..), RolInContext, Value, binding, genericBinding, getProperty, getUnqualifiedProperty, typeWithPerspectivesTypes)
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..))
import Prelude (class Eq, bind, id, join, pure, show, ($), (<>), (==), (>=>), (>>=), (>>>), (<<<), map)

-----------------------------------------------------------
-- GETROL, GETPROPERTY
-----------------------------------------------------------
-- | Get the RolInContext instances with the given rol name (RolDef) directly from the Context definition (not searching prototypes or Aspects).
-- | E.g. getRol "model:Perspectives$View$rolProperty" will return all rol instances that bind a PropertyDef on an instance of psp:View.
getContextRol :: forall e. RolDef -> (AnyContext ~~> ContextRol) e
getContextRol rn = typeWithPerspectivesTypes $ getContextMember \context -> maybe [] id (lookup (unwrap rn) (context_rolInContext context))

getRolInContext :: forall e. RolDef -> (AnyContext ~~> RolInContext) e
getRolInContext = typeWithPerspectivesTypes getContextRol

-- | Get the RolInContext instances with the given local name directly from the Context definition (not searching prototypes of Aspects).
-- E.g. getUnqualifiedRol "rolProperty" will return the same result as getRol "model:Perspectives$View$rolProperty".
-- TODO: rename getRolByLocalName to getUnqualifiedRol.
getUnqualifiedContextRol :: forall e. Id.LocalName -> (Context ~~> ContextRol) e
getUnqualifiedContextRol ln = typeWithPerspectivesTypes $ getContextMember \context -> maybe [] id (lookup (ln `qualifiedWith` context) (context_rolInContext context))
  where
    qualifiedWith :: Id.LocalName -> PerspectContext -> String
    qualifiedWith ln (PerspectContext {pspType}) = pspType <> "$" <> ln

getUnqualifiedRolInContext :: forall e. Id.LocalName -> (Context ~~> RolInContext) e
getUnqualifiedRolInContext = typeWithPerspectivesTypes getUnqualifiedContextRol

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

-- | Combinator to make an ObjectsGetter fail if it returns an empty result.
-- | Useful in combination with computing alternatives using <|>
unlessNull :: forall s o e. (s ~~> o) e -> (s ~~> o) e
unlessNull og id = og id >>= \r -> if (null r) then empty else pure r

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
searchInAspectHierarchy :: forall o e. Eq o => (BuitenRol ~~> o) e -> (AnyContext ~~> o) e
searchInAspectHierarchy getter contextId =
  unlessNull (searchLocallyAndInPrototypeHierarchy getter) contextId
  <|>
  (directAspects /-/ searchInAspectHierarchy getter) contextId

directAspects :: forall e. (AnyContext ~~> AnyContext) e
directAspects = getContextRol (RolDef "model:Perspectives$Context$aspect") /-/ rolBindingDef

directAspectRoles :: forall e. (RolDef ~~> RolDef) e
directAspectRoles = typeWithPerspectivesTypes $ getContextRol (RolDef "model:Perspectives$Rol$aspectRol") /-/ rolBindingDef

concat :: forall e. ObjectsGetter e -> ObjectsGetter e -> ObjectsGetter e
concat f p s = do
  fs <- f s
  ps <- p s
  pure $ union fs ps

-- | True iff at least one of the boolean results of f is true (where true is represented as PBool "true").
some :: forall e. (String ~~> PBool) e -> (String ~~> PBool) e
some f = f `composeMonoidal` (alaF Disj foldMap ((==) (PBool "true")) >>> show >>> PBool)

-- | True iff at all of the boolean results of f is true (where true is represented as PBool "true").
all :: forall s e. (s ~~> PBool) e -> (s ~~> PBool) e
all f = f `composeMonoidal` (alaF Conj foldMap ((==) (PBool "true")) >>> show >>> PBool)
-----------------------------------------------------------
-- OTHER CONSTRUCTORS
-----------------------------------------------------------
-- | All role instances in the telescope, excluding the root.
closureOfBinding :: forall r e. Binding r r => (r ~~> r) e
closureOfBinding = closure binding

-- | The value of the property pd, wherever in the telescope it is represented.
searchQualifiedProperty :: forall b e. RolClass b => PropertyDef -> (b ~~> Value) e
searchQualifiedProperty pd = typeWithPerspectivesTypes searchInRolTelescope g
  where
    g :: (b ~~> Value) e
    g = getProperty pd

-- | The value of the unqualified property pd, wherever in the telescope it is represented.
-- | NOTE: this function cannot be applied to a BinnenRol.
searchUnQualifiedProperty :: forall b e. RolClass b => Id.LocalName -> (b ~~> Value) e
searchUnQualifiedProperty pd = typeWithPerspectivesTypes searchInRolTelescope g
  where
    g :: (b ~~> Value) e
    g = getUnqualifiedProperty pd

-- | All Aspects of a ContextType, excluding the ContextType itself.
closureOfAspect :: forall e. ObjectsGetter e
closureOfAspect = closure directAspects

-- | All AspectRollen of a RolDef, excluding the RolDef itself.
closureOfAspectRol :: forall e. (RolDef ~~> RolDef) e
closureOfAspectRol = closure directAspectRoles

-- | The prototype of a ContextType.
getPrototype :: forall e. (ContextDef ~~> ContextDef) e
getPrototype = typeWithPerspectivesTypes (buitenRol /-/ binding /-/ context)

-- | All prototypes of a ContextType, excluding the ContextType itself.
closureOfPrototype :: forall e. (ContextDef ~~> ContextDef) e
closureOfPrototype = closure getPrototype

-- | Look for the definition of a Rol by its local name, in the ContextType (not searching prototypes or Aspects).
-- | If no Rol is defined with this local name, will return an empty result.
searchRolDefinitionLocally ::	forall e. Id.LocalName -> (ContextDef ~~> RolDef) e
searchRolDefinitionLocally = typeWithPerspectivesTypes getUnqualifiedContextRol

-- | Look for the definition of a Rol by its local name, in the Aspects of the ContextType and in all prototypes.
-- TODO: replace uses of 'getRolUsingAspects' by this function.
-- getRolUsingAspects `psp:Rol -> ObjectsGetter`
searchRolDefinitionInAspects ::	forall e. Id.LocalName -> (ContextDef ~~> RolDef) e
searchRolDefinitionInAspects ln = unwrap >>> searchInAspectHierarchy f
  where
    f :: (BuitenRol ~~> RolDef) e
    f = context >=> pure <<< map ContextDef /-/ searchRolDefinitionLocally ln

-- | Look for the ContextRol instances with the given local name both in the Context definition and its prototypes.
-- TODO: hernoem getRolFromPrototypeHierarchy naar searchRolLocallyAndInPrototypeHierarchy
-- OF: let op of niet searchRolDefinitionInAspects gebruikt moet worden (mogelijke fout in aanroepende code!)
searchRolLocallyAndInPrototypeHierarchy :: forall e. Id.LocalName -> (ContextDef ~~> ContextRol) e
searchRolLocallyAndInPrototypeHierarchy rn = unwrap >>> searchLocallyAndInPrototypeHierarchy (context /-/ (Context >>> (getUnqualifiedContextRol rn) :: (Context ~~> ContextRol) e))

-- | Searches the property with the local name first in the telescope of the Role.
-- | Then searches the property on the instance of the same role on the prototypes.
-- | This is **not** searching on aspectRoles!
-- | Use this function if the ContextType c in your model has a prototype with the role RolDef
-- | but additionally provides properties on its own version of that role.
searchPropertyOfRol :: forall e. RolDef -> Id.LocalName -> (Context ~~> Value) e
searchPropertyOfRol r p = unwrap >>> searchLocallyAndInPrototypeHierarchy f
  where
    f :: (BuitenRol ~~> Value) e
    f = (context /-/ (getContextRol r) /-/ ((searchUnQualifiedProperty p) :: (ContextRol ~~> Value) e))

-- | Look for the property PropertyDef on the buitenRol of the ContextType c and on its telescope.
-- TODO: rename getExternalProperty to searchExternalProperty
getExternalProperty :: forall e. PropertyDef -> (AnyContext ~~> Value) e
getExternalProperty pn = buitenRol /-/ searchQualifiedProperty pn

-- | Look for the property with the given local name on the buitenRol of the ContextType c and on its telescope.
-- TODO: rename lookupExternalProperty to searchExternalUnqualifiedProperty
lookupExternalProperty :: forall e. Id.LocalName -> (AnyContext ~~> Value) e
lookupExternalProperty ln = buitenRol /-/ searchUnQualifiedProperty ln

-- | Look for the property with the given local name on the binnenRol of the ContextType c.
getInternalProperty :: forall e. PropertyDef -> (AnyContext ~~> Value) e
getInternalProperty pn = binnenRol /-/ getProperty pn

-- | Look for the property with the given local name on the binnenRol of the ContextType c and on its telescope.
lookupInternalProperty :: forall e. Id.LocalName -> (AnyContext ~~> Value) e
lookupInternalProperty ln = binnenRol /-/ searchUnQualifiedProperty ln

-- | From the instance of a Rol of any kind, find the instances of the Rol of the given type that bind it (that have
-- | it as their binding).
getGebondenAls :: forall r b e. RolClass r => RolClass b => RolDef -> (r ~~> b) e
getGebondenAls rname = typeWithPerspectivesTypes $ getRolMember \(PerspectRol{gevuldeRollen}) -> maybe [] id (lookup (unwrap rname) gevuldeRollen)
