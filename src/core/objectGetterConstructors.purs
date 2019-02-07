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
import Perspectives.ContextAndRole (context_binnenRol, context_rolInContext, rol_properties)
import Perspectives.ContextRolAccessors (getContextMember, getContextMember', getRolMember)
import Perspectives.CoreTypes (TypedObjectsGetter, type (~~>), MP)
import Perspectives.DataTypeObjectGetters (binding, buitenRol, context)
import Perspectives.Identifiers (LocalName) as Id
import Perspectives.ObjectsGetterComposition (composeMonoidal, (/-/))
import Perspectives.PerspectivesTypesInPurescript (class Binding, class ContextType, class RolKind, BuitenRol, PBool(..), PropertyDef, RolDef(..), RolInContext, class Val, typeWithPerspectivesTypes, BuitenRol(..))
import Perspectives.Syntax (PerspectContext(..), PerspectRol(..), propertyValue)
import Prelude (class Eq, bind, id, join, otherwise, pure, show, ($), (<>), (==), (>=>), (>>=), (>>>))

-----------------------------------------------------------
-- COMBINATORS
-----------------------------------------------------------
-- | The closure of a TypedObjectsGetter (cumulates results, excluding the root).
closure :: forall s e.
  Eq s =>
  TypedObjectsGetter s s e ->
  TypedObjectsGetter s s e
closure p = getter [] where
  getter :: Array s -> s -> MP e (Array s)
  getter cumulator id = do
    (objectsOfP :: Array s) <- p id
    case elemIndex id cumulator of
      Nothing -> do
        (results :: Array (Array s)) <- traverse (getter (union cumulator objectsOfP)) (difference objectsOfP cumulator)
        pure $ nub $ join (cons objectsOfP results)
      otherwise -> pure objectsOfP

searchInRolTelescope :: forall s o e. Eq o => RolKind s => Binding s => (s ~~> o) e -> (s ~~> o) e
searchInRolTelescope getter rolId =
  unlessNull getter rolId
  <|>
  ((binding :: (s ~~> s) e) /-/ searchInRolTelescope getter) rolId

-- | Applies the getter (s ~~> o) e to each higher prototype until it succeeds or there is no prototype.
-- | Does *not* apply the getter to the ContextType that is passed in!
searchInPrototypeHierarchy :: forall s o e. Eq o => ContextType s => (s ~~> o) e -> (s ~~> o) e
searchInPrototypeHierarchy getter = buitenRol /-/ (searchInRolTelescope context /-/ getter)

-- | Combinator to make an ObjectsGetter fail if it returns an empty result.
-- | Useful in combination with computing alternatives using <|>
unlessNull :: forall s o e. (s ~~> o) e -> (s ~~> o) e
unlessNull og id = og id >>= \r -> if (null r) then empty else pure r

-- | Applies the getter (s ~~> o) e to the ContextType and all its prototypes.
searchLocallyAndInPrototypeHierarchy :: forall s o e. Eq o => ContextType s => (s ~~> o) e -> (s ~~> o) e
searchLocallyAndInPrototypeHierarchy getter c = unlessNull getter c <|> searchInPrototypeHierarchy getter c

-- | Applies the getter (s ~~> o) e to the ContextType and all its prototypes and recursively to all its aspects.
searchInAspectHierarchy :: forall c o e. Eq o => ContextType c => (c ~~> o) e -> (c ~~> o) e
searchInAspectHierarchy getter contextId =
  unlessNull (searchLocallyAndInPrototypeHierarchy getter) contextId
  <|>
  ((directAspects :: (c ~~> c) e) /-/ searchInAspectHierarchy getter) contextId

directAspects :: forall t e. ContextType t => (t ~~> t) e
directAspects = getRol (RolDef "model:Perspectives$Context$aspect") /-/ binding /-/ context

directAspectRoles :: forall e. (RolDef ~~> RolDef) e
directAspectRoles = getRol (RolDef "model:Perspectives$Rol$aspectRol") /-/ binding /-/ context

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
-- GETROL, GETPROPERTY
-----------------------------------------------------------
-- | Get the RolInContext instances with the given rol name (RolDef) directly from the Context definition (not searching prototypes or Aspects).
-- | E.g. getRol "model:Perspectives$View$rolProperty" will return all rol instances that bind a PropertyDef on an instance of psp:View.
getRol :: forall c e. ContextType c => RolDef -> (c ~~> RolInContext) e
getRol rn = typeWithPerspectivesTypes $ getContextMember \context -> maybe [] id (lookup (unwrap rn) (context_rolInContext context))

-- | Get the RolInContext instances with the given local name directly from the Context definition (not searching prototypes of Aspects).
-- E.g. getUnqualifiedRol "rolProperty" will return the same result as getRol "model:Perspectives$View$rolProperty".
-- TODO: rename getRolByLocalName to getUnqualifiedRol.
getUnqualifiedRol :: forall c e. ContextType c => Id.LocalName -> (c ~~> RolInContext) e
getUnqualifiedRol ln = typeWithPerspectivesTypes $ getContextMember \context -> maybe [] id (lookup (ln `qualifiedWith` context) (context_rolInContext context))
  where
    qualifiedWith :: Id.LocalName -> PerspectContext -> String
    qualifiedWith ln (PerspectContext {pspType}) = pspType <> "$" <> ln

-- | Get the values for the property PropertyDef that are directly represented on the instance of a rol of type r
-- | E.g. getProperty "model:Perspectives$Systeem$gebruiker$voornaam"
getProperty :: forall r v e. RolKind r => Val v => PropertyDef -> (r ~~> v) e
getProperty pn = typeWithPerspectivesTypes $ getRolMember \rol -> maybe [] propertyValue (lookup (unwrap pn) (rol_properties rol))

-- | Get the values for the property with the local name that are directly represented on the instance of a rol of type r
-- | E.g. getUnqualifiedProperty "voornaam"
getUnqualifiedProperty :: forall r v e. RolKind r => Val v => Id.LocalName -> (r ~~> v) e
getUnqualifiedProperty ln = typeWithPerspectivesTypes $ getRolMember \rol -> maybe [] propertyValue (lookup (ln `qualifiedWith` rol) (rol_properties rol)) -- Moet gekwalificeerd zijn met het type van de rol!
  where
    qualifiedWith :: Id.LocalName -> PerspectRol -> String
    qualifiedWith ln (PerspectRol {pspType}) = pspType <> "$" <> ln

-----------------------------------------------------------
-- OTHER CONSTRUCTORS
-----------------------------------------------------------
-- | All role instances in the telescope, excluding the root.
closureOfBinding :: forall rt e. Binding rt => (rt ~~> rt) e
closureOfBinding = closure binding

-- | The value of the property pd, wherever in the telescope it is represented.
-- | NOTE: this function cannot be applied to a BinnenRol.
searchQualifiedProperty :: forall b v e. Binding b => Val v => PropertyDef -> (b ~~> v) e
searchQualifiedProperty pd = searchInRolTelescope $ getProperty pd

-- | The value of the unqualified property pd, wherever in the telescope it is represented.
-- | NOTE: this function cannot be applied to a BinnenRol.
searchUnQualifiedProperty :: forall rt v e. Binding rt => Val v => Id.LocalName -> (rt ~~> v) e
searchUnQualifiedProperty pd = searchInRolTelescope $ getUnqualifiedProperty pd

-- | All Aspects of a ContextType, excluding the ContextType itself.
closureOfAspect :: forall ct e. ContextType ct => (ct ~~> ct) e
closureOfAspect = closure directAspects

-- | All AspectRollen of a RolDef, excluding the RolDef itself.
closureOfAspectRol :: forall e. (RolDef ~~> RolDef) e
closureOfAspectRol = closure directAspectRoles

-- | The prototype of a ContextType.
getPrototype :: forall ct e. ContextType ct => (ct ~~> ct) e
getPrototype = buitenRol /-/ (binding :: (BuitenRol ~~> BuitenRol) e) /-/ context

-- | All prototypes of a ContextType, excluding the ContextType itself.
closureOfPrototype :: forall ct e. ContextType ct => (ct ~~> ct) e
closureOfPrototype = closure getPrototype

-- | Look for the definition of a Rol by its local name, in the ContextType (not searching prototypes or Aspects).
-- | If no Rol is defined with this local name, will return an empty result.
searchRolDefinitionLocally ::	forall c e. ContextType c => Id.LocalName -> (c ~~> RolDef) e
searchRolDefinitionLocally ln c = ((getRol (RolDef "model:Perspectives$Context$rolInContext") /-/ binding /-/ context) >=>
  (\(definedRoles :: Array RolDef) -> case elemIndex (ln `qualifiedWith` c) definedRoles of
    Nothing -> pure []
    otherwise -> pure [(ln `qualifiedWith` c)])) c

  where
    qualifiedWith :: Id.LocalName -> c -> RolDef
    qualifiedWith ln contextId = RolDef $ (unwrap contextId) <> ln

-- | Look for the definition of a Rol by its local name, in the Aspects of the ContextType and in all prototypes.
-- TODO: replace uses of 'getRolUsingAspects' by this function.
-- getRolUsingAspects `psp:Rol -> ObjectsGetter`
searchRolDefinitionInAspects ::	forall c e. ContextType c => Id.LocalName -> (c ~~> RolDef) e
searchRolDefinitionInAspects ln = searchInAspectHierarchy $ searchRolDefinitionLocally ln

-- | Look for the RolInContext instances with the given local name both in the Context definition and its prototypes.
-- | E.g.: searchRolLocallyAndInPrototypeHierarchy "binnenRolBeschrijving" "model:Perspectives$View"
-- | will actually find the description of the BinnenRol in the prototype of psp:View.
-- TODO: hernoem getRolFromPrototypeHierarchy naar searchRolLocallyAndInPrototypeHierarchy
-- OF: let op of niet searchRolDefinitionInAspects gebruikt moet worden (mogelijke fout in aanroepende code!)
searchRolLocallyAndInPrototypeHierarchy :: forall c e. ContextType c => Id.LocalName -> (c ~~> RolInContext) e
searchRolLocallyAndInPrototypeHierarchy rn = searchLocallyAndInPrototypeHierarchy (getUnqualifiedRol rn)

-- | Searches the property first on an eventual locally represented version of the Role, then on its version on the prototypes.
-- | Use this function if the ContextType c in your model has a prototype with the role RolDef but additionally provides properties on its own version of that role.
searchPropertyOfRol :: forall c v e. ContextType c => Val v => RolDef -> Id.LocalName -> (c ~~> v) e
searchPropertyOfRol r p = searchLocallyAndInPrototypeHierarchy (getRol r) /-/ (searchUnQualifiedProperty p)

-- | Look for the property PropertyDef on the buitenRol of the ContextType c and on its telescope.
getExternalProperty :: forall c v e. ContextType c => Val v => PropertyDef -> (c ~~> v) e
getExternalProperty pn = buitenRol /-/ searchQualifiedProperty pn

-- | Look for the property with the given local name on the buitenRol of the ContextType c and on its telescope.
lookupExternalProperty :: forall c v e. ContextType c => Val v => Id.LocalName -> (c ~~> v) e
lookupExternalProperty ln = buitenRol /-/ searchUnQualifiedProperty ln

-- | Look for the property with the given local name on the binnenRol of the ContextType c.
-- TODO: this used to be a qualified name.
getInternalProperty :: forall c v e. ContextType c => Val v => Id.LocalName -> (c ~~> v) e
getInternalProperty ln ident = typeWithPerspectivesTypes $ do
  (mbr :: Maybe PerspectRol) <- getContextMember' context_binnenRol (unwrap ident)
  case mbr of
    Nothing -> pure []
    -- TODO: vervang de pattern matching zodra binnenRol een 'echte' rol is.
    (Just rol) -> pure $ (maybe [] propertyValue) (lookup ln (rol_properties rol))

-- | Look for the property PropertyDef on the binnenRol of the ContextType c and on its roltelescope (that consists of
-- | the buitenRol and its bindings, i.e. the prototype hierachy).
-- | Notice that a property defined on the binnenRol will have a different namespace than a property defined on the
-- | the buitenRol. Therefore we search with the local name.
lookupInternalProperty :: forall c v e. ContextType c => Val v => Id.LocalName -> (c ~~> v) e
lookupInternalProperty ln ident = unlessNull (getInternalProperty ln) ident <|> lookupExternalProperty ln ident

-- | From the instance of a Rol of any kind, find the instances of the Rol of the given type that bind it (that have
-- | it as their binding).
getGebondenAls :: forall r e. RolKind r => RolDef -> (r ~~> RolInContext) e
getGebondenAls rname = typeWithPerspectivesTypes $ getRolMember \(PerspectRol{gevuldeRollen}) -> maybe [] id (lookup (unwrap rname) gevuldeRollen)
