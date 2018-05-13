module Perspectives.Property where

import Control.Alt ((<|>))
import Control.Plus (empty)
import Control.Monad.Eff.Exception (error)
import Data.Array (foldl, head, nub, singleton, null)
import Data.Array.Partial (head) as ArrayPartial
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap (keys, lookup, values)
import Partial.Unsafe (unsafePartial)
import Perspectives.ContextAndRole (context_binnenRol, context_buitenRol, context_displayName, context_id, context_pspType, context_rolInContext, rol_binding, rol_context, rol_properties, rol_pspType)
import Perspectives.CoreTypes (MonadPerspectives, ObjectsGetter, ObjectGetter)
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.EntiteitAndRDFAliases (ContextID, ID, PropertyName, RolName, RolID)
import Perspectives.Identifiers (LocalName, buitenRol, deconstructNamespace)
import Perspectives.PropertyComposition ((/-/))
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.Syntax (PerspectContext, PerspectRol(..), propertyValue)
import Perspectives.Utilities (onNothing)
import Prelude (bind, id, join, pure, show, ($), (<$>), (<<<), (<>), (==), (>=>), (||), (>>=))

{-
Property values are represented by Arrays.
We need functions that give us an array of values for a given property for a given resource.
-}

getContextMember :: forall e. (PerspectContext -> Array String) -> ObjectsGetter e
getContextMember f c = do
  maybeContext <- getPerspectEntiteit c
  case maybeContext of
    (Just perspectContext) -> pure $ f perspectContext
    otherwise -> pure []

-- Even though members of a context will always be present, the context itself may not. Hence we return a Maybe value.
getContextMember' :: forall a e. (PerspectContext -> a) -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe a))
getContextMember' f c = do
  maybeContext <- getPerspectEntiteit c
  case maybeContext of
    (Just perspectContext) -> pure $ Just $ f perspectContext
    otherwise -> pure Nothing

getContextType :: forall e. ObjectsGetter e
getContextType = getContextMember \context -> [context_pspType context]

getContextTypeF :: forall e. ObjectGetter e
getContextTypeF = makeFunction "getContextTypeF" getContextType

makeFunction :: forall e. String -> ObjectsGetter e -> ObjectGetter e
makeFunction name og = og >=> (\ta -> onNothing (error $ "Function yields no value: " <> name) (pure (head ta)))

firstOnly :: forall e. ObjectsGetter e -> (ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String))
firstOnly g = g >=> (pure <<< head)

-- Returns an empty array if the context does not exist.
getBuitenRol :: forall e. ObjectsGetter e
getBuitenRol = getContextMember \c -> [context_buitenRol c]

-- Returns Nothing if the context does not exist.
getBuitenRol' :: forall e. ID -> MonadPerspectives (AjaxAvarCache e) (Maybe String)
getBuitenRol' = getContextMember' \c -> context_buitenRol c

getRol :: forall e. RolName -> ObjectsGetter e
getRol rn = getContextMember \context -> maybe [] id (lookup rn (context_rolInContext context))

rolNameInContext :: LocalName -> ContextID -> RolName
rolNameInContext ln contextId = (maybe "" id (deconstructNamespace contextId)) <> "$" <> ln

getRolByLocalName :: forall e. RolName -> ObjectsGetter e
getRolByLocalName rn = getContextMember \context -> maybe [] id (lookup (rolNameInContext rn (context_id context)) (context_rolInContext context))

-- | Given a qualified name of a Rol, return that Rol from the context or recursively from its prototype.
getRolFromPrototypeHierarchy :: forall e. RolName -> ObjectsGetter e
getRolFromPrototypeHierarchy rn contextId =
  unlessNull (getRol rn) contextId
  <|>
  (getBuitenRol /-/ getRolBinding /-/ getRolContext /-/ getRolFromPrototypeHierarchy rn) contextId

getRollen :: forall e. ObjectsGetter e
getRollen = getContextMember \context -> nub $ join $ values (context_rolInContext context)

-- | The names of every rol given to this context.
getRolTypen :: forall e. ObjectsGetter e
getRolTypen = getContextMember \context -> keys (context_rolInContext context)

-- | The names of every property given to this rol.
getPropertyTypen :: forall e. ObjectsGetter e
getPropertyTypen = getRolMember \rol -> keys (rol_properties rol)

-- | The names of every internal property given to this context.
getInternePropertyTypen :: forall e. ObjectsGetter e
getInternePropertyTypen = getContextMember \context -> keys (rol_properties (context_binnenRol context))

getDisplayName :: forall e. ObjectsGetter e
getDisplayName = getContextMember \context -> [(context_displayName context)]

getExternalProperty :: forall e. PropertyName -> ObjectsGetter e
getExternalProperty pn id = do
  mbr <- getBuitenRol' id
  case mbr of
    Nothing -> pure []
    (Just br) -> getProperty pn br

-- | Look up a local name in the rol telescope of the buitenrol.
lookupExternalProperty :: forall e. LocalName -> ObjectsGetter e
lookupExternalProperty pn id = getPropertyFromRolTelescope pn $ buitenRol id

getInternalProperty :: forall e. PropertyName -> ObjectsGetter e
getInternalProperty pn ident = do
  (mbr :: Maybe PerspectRol) <- getContextMember' context_binnenRol ident
  case mbr of
    Nothing -> pure []
    -- TODO: vervang de pattern matching zodra binnenRol een 'echte' rol is.
    (Just rol) -> pure $ (maybe [] propertyValue) (lookup pn (rol_properties rol))

-- | Look up a local name in the rol telescope of the binnenrol.
lookupInternalProperty :: forall e. LocalName -> ObjectsGetter e
lookupInternalProperty pn id =
  unlessNull (getInternalProperty pn) id
  <|>
  (getRolBinding /-/ getPropertyFromRolTelescope pn) id

-- | Combinator to make an ObjectsGetter fail if it returns an empty result.
-- | Useful in combination with computing alternatives using <|>
unlessNull :: forall e. ObjectsGetter e -> ObjectsGetter e
unlessNull og id = og id >>= \r -> if (null r) then empty else pure r

getRolMember :: forall e. (PerspectRol -> Array String) -> ObjectsGetter e
getRolMember f c = do
  maybeRol <- getPerspectEntiteit c
  case maybeRol of
    (Just perspectRol) -> pure $ f perspectRol
    otherwise -> pure []

getRolType :: forall e. ObjectsGetter e
getRolType = getRolMember \rol -> [rol_pspType rol]

getRolBinding :: forall e. ObjectsGetter e
getRolBinding = getRolMember \rol -> maybe [] singleton (rol_binding rol)

-- | From the instance of a Rol, find the instances of the Rol of the given type that bind it (has it as their binding).
getGebondenAls :: forall e. RolName -> ObjectsGetter e
getGebondenAls rname = getRolMember \(PerspectRol{gevuldeRollen}) -> maybe [] id (lookup rname gevuldeRollen)

getRolContext :: forall e. ObjectsGetter e
getRolContext = getRolMember \rol -> [rol_context rol]

getProperty :: forall e. PropertyName -> ObjectsGetter e
getProperty pn = getRolMember \rol -> maybe [] propertyValue (lookup pn (rol_properties rol))

-- | In the roltelescope, find a property with a given qualified name.
-- | NOTE: This function will loop whenever a RolInstance binds to itself!
getPropertyFromRolTelescope :: forall e. PropertyName -> ObjectsGetter e
getPropertyFromRolTelescope qn rolId =
  unlessNull (getProperty qn) rolId
  <|>
  (getRolBinding /-/ getPropertyFromRolTelescope qn) rolId

-- | Some ObjectsGetters will return an array with a single ID. Some of them represent contexts (such as the result
-- | of getRolContext), others roles (such as the result of getRolBinding). The Partial function below returns that
-- | single ID instead of the Array holding it, effectively turning an ObjectsGetter into an ObjectGetter.
toSingle :: forall e. Partial => ObjectsGetter e -> ObjectGetter e
toSingle og id = do
  (ar :: Array String) <- og id
  pure $ ArrayPartial.head ar

getRolBinding' :: forall e. ObjectGetter e
getRolBinding' = unsafePartial $ toSingle getRolBinding

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
propertyIsVerplicht :: forall e. ObjectsGetter e
propertyIsVerplicht = booleanPropertyGetter "model:Perspectives$Context$aspectProperty"
  "model:Perspectives$Property$isVerplicht"

-- | Equal to the 'own' $isFunctioneel value; otherwise the logical or of the #aspectProperty values.
propertyIsFunctioneel :: forall e. ObjectsGetter e
propertyIsFunctioneel = booleanPropertyGetter "model:Perspectives$Context$aspectProperty" "model:Perspectives$Property$isFunctioneel"

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
rolIsVerplicht :: forall e. ObjectsGetter e
rolIsVerplicht = booleanPropertyGetter "model:Perspectives$Context$aspect"
  "model:Perspectives$Rol$isVerplicht"

-- | Equal to the 'own' $isVerplicht value; otherwise the logical or of the #aspectProperty values.
rolIsFunctioneel :: forall e. ObjectsGetter e
rolIsFunctioneel = booleanPropertyGetter "model:Perspectives$Context$aspect"
  "model:Perspectives$Rol$isFunctioneel"

-- | Using either $aspect or $aspectProperty, climb the Aspect tree looking
-- | for a Boolean Property bearing the given propertyName.
-- | `psp:Rol -> psp:Property -> ObjectsGetter`
booleanPropertyGetter :: forall e. RolID -> PropertyName -> ObjectsGetter e
booleanPropertyGetter aspectRol propertyName = getter where
  getter :: ObjectsGetter e
  getter pid =
    unlessNull (getExternalProperty propertyName) pid
    <|>
    (getRol aspectRol /-/ getRolBinding /-/ getRolContext /-/ getter) pid >>=
      \r -> pure [show $ foldl (||) false ((==) "true" <$> r)]

-- | Climb the Aspect tree looking for a Rol bearing the given name.
-- | `psp:Rol -> ObjectsGetter`
getRolUsingAspects :: forall e. RolName -> ObjectsGetter e
getRolUsingAspects rolName = getter where
  getter :: ObjectsGetter e
  getter pid =
    unlessNull (getRol rolName) pid
    <|>
    (getRol "model:Perspectives$Rol$aspectRol" /-/ getRolBinding /-/ getRolContext /-/ getter) pid
