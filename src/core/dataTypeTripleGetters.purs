module Perspectives.DataTypeTripleGetters where

import Perspectives.CoreTypes (ObjectsGetter, type (**>))
import Perspectives.DataTypeObjectGetters (buitenRol, contextType, label, context, rolType, typeVanIedereRolInContext, iedereRolInContext)
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, AnyDefinition, BuitenRol, RolDef, binding)
import Perspectives.TripleGetterConstructors (constructTripleGetterFromObjectsGetter, trackedAs)
import Prelude (pure)

identity :: forall e. ObjectsGetter e
identity x = pure [x]

-- | Identity for all values, contexts and roles.
identityM :: forall e. (String **> String) e
identityM = constructTripleGetterFromObjectsGetter "model:Perspectives$identity" identity

-- | The type of the context instance.
contextTypeM :: forall e. (AnyContext **> AnyDefinition) e
contextTypeM = contextType `trackedAs` "model:Perspectives$type"

-- | The type of the rol instance.
rolTypeM :: forall r e. RolClass r => (r **> RolDef) e
rolTypeM = rolType `trackedAs` "model:Perspectives$type"

buitenRolM :: forall e. (AnyContext **> BuitenRol) e
buitenRolM = buitenRol `trackedAs` "model:Perspectives$buitenRol"

-- | Every rol instance belonging to the context instance.
iedereRolInContextM :: forall e. (String **> String) e
iedereRolInContextM = iedereRolInContext `trackedAs`  "model:Perspectives$iedereRolInContext"

-- | The types of the rol instances given to this context instance. Note: non-mandatory
-- | Rol types defined for the Context type may be missing!
typeVanIedereRolInContextM :: forall e. (String **> String) e
typeVanIedereRolInContextM =  typeVanIedereRolInContext `trackedAs` "model:Perspectives$typeVanIedereRolInContext"

-- | The rol instance that this rol instance is bound to, i.e. the head of its telescope.
-- | `psp:RolInstance -> psp:RolInstance`
bindingM :: forall binder bound e. Binding binder bound => (binder **> bound) e
bindingM = binding `trackedAs` "model:Perspectives$binding"

-- | The context instance of the rol instance.
-- | `psp:RolInstance -> psp:ContextInstance`
contextM :: forall r e. RolClass r => (r **> String) e
contextM = context `trackedAs` "model:Perspectives$context"

-- | The string that labels the context instance.
labelM :: forall e. (AnyContext **> String) e
labelM = label `trackedAs` "model:Perspectives$label"
