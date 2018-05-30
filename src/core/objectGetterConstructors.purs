module Perspectives.ObjectGetterConstructors where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (foldl, null)
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (lookup)
import Perspectives.ContextAndRole (context_binnenRol, context_pspType, context_rolInContext, rol_properties)
import Perspectives.ContextRolAccessors (getContextMember, getContextMember', getRolMember)
import Perspectives.CoreTypes (ObjectsGetter)
import Perspectives.EntiteitAndRDFAliases (PropertyName, RolName, RolID)
import Perspectives.Identifiers (LocalName, buitenRol) as Id
import Perspectives.ObjectsGetterComposition ((/-/))
import Perspectives.Syntax (PerspectRol(..), propertyValue)
import Perspectives.DataTypeObjectGetters (buitenRol, buitenRol', binding, context)
import Prelude (bind, id, pure, show, ($), (<$>), (<>), (==), (||), (>>=))

getRol :: forall e. RolName -> ObjectsGetter e
getRol rn = getContextMember \context -> maybe [] id (lookup rn (context_rolInContext context))

-- | Lookup a local rol name by prefixing it with the `type` of the
-- | context. Returns the RolInstance, not its binding.
getRolByLocalName :: forall e. RolName -> ObjectsGetter e
getRolByLocalName rn = getContextMember \context -> maybe [] id (lookup ((context_pspType context) <> "$" <> rn) (context_rolInContext context))

-- | Given a qualified name of a Rol, return that Rol from the context or recursively from its prototype.
getRolFromPrototypeHierarchy :: forall e. RolName -> ObjectsGetter e
getRolFromPrototypeHierarchy rn contextId =
  unlessNull (getRol rn) contextId
  <|>
  (buitenRol /-/ binding /-/ context /-/ getRolFromPrototypeHierarchy rn) contextId

getExternalProperty :: forall e. PropertyName -> ObjectsGetter e
getExternalProperty pn id = do
  mbr <- buitenRol' id
  case mbr of
    Nothing -> pure []
    (Just br) -> getProperty pn br

-- | Look up a local name in the rol telescope of the buitenrol.
lookupExternalProperty :: forall e. Id.LocalName -> ObjectsGetter e
lookupExternalProperty pn id = getPropertyFromRolTelescope pn $ Id.buitenRol id

getInternalProperty :: forall e. PropertyName -> ObjectsGetter e
getInternalProperty pn ident = do
  (mbr :: Maybe PerspectRol) <- getContextMember' context_binnenRol ident
  case mbr of
    Nothing -> pure []
    -- TODO: vervang de pattern matching zodra binnenRol een 'echte' rol is.
    (Just rol) -> pure $ (maybe [] propertyValue) (lookup pn (rol_properties rol))

-- | Look up a local name in the rol telescope of the binnenrol.
lookupInternalProperty :: forall e. Id.LocalName -> ObjectsGetter e
lookupInternalProperty pn id =
  unlessNull (getInternalProperty pn) id
  <|>
  (binding /-/ getPropertyFromRolTelescope pn) id

-- | Combinator to make an ObjectsGetter fail if it returns an empty result.
-- | Useful in combination with computing alternatives using <|>
unlessNull :: forall e. ObjectsGetter e -> ObjectsGetter e
unlessNull og id = og id >>= \r -> if (null r) then empty else pure r

-- | From the instance of a Rol, find the instances of the Rol of the given type that bind it (has it as their binding).
getGebondenAls :: forall e. RolName -> ObjectsGetter e
getGebondenAls rname = getRolMember \(PerspectRol{gevuldeRollen}) -> maybe [] id (lookup rname gevuldeRollen)

getProperty :: forall e. PropertyName -> ObjectsGetter e
getProperty pn = getRolMember \rol -> maybe [] propertyValue (lookup pn (rol_properties rol))

-- | In the roltelescope, find a property with a given qualified name.
-- | NOTE: This function will loop whenever a RolInstance binds to itself!
getPropertyFromRolTelescope :: forall e. PropertyName -> ObjectsGetter e
getPropertyFromRolTelescope qn rolId =
  unlessNull (getProperty qn) rolId
  <|>
  (binding /-/ getPropertyFromRolTelescope qn) rolId

-- | Using either $aspect or $aspectProperty, climb the Aspect tree looking
-- | for a Boolean Property bearing the given propertyName.
-- | `psp:Rol -> psp:Property -> ObjectsGetter`
booleanPropertyGetter :: forall e. RolID -> PropertyName -> ObjectsGetter e
booleanPropertyGetter aspectRol propertyName = getter where
  getter :: ObjectsGetter e
  getter pid =
    unlessNull (getExternalProperty propertyName) pid
    <|>
    (getRol aspectRol /-/ binding /-/ context /-/ getter) pid >>=
      \r -> pure [show $ foldl (||) false ((==) "true" <$> r)]

-- | Climb the Aspect tree looking for a Rol bearing the given name.
-- | Uses getRolFromPrototypeHierarchy internally, so caters for prototyping.
-- | `psp:Rol -> ObjectsGetter`
getRolUsingAspects :: forall e. RolName -> ObjectsGetter e
getRolUsingAspects rolName contextId =
  unlessNull (getRolFromPrototypeHierarchy rolName) contextId
    <|>
    (getRol "model:Perspectives$Rol$aspectRol" /-/ binding /-/ context /-/ getRolUsingAspects rolName) contextId
