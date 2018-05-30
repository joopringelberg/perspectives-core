module Perspectives.DataTypeTripleGetters where

import Perspectives.CoreTypes (ObjectsGetter, TypedTripleGetter)
import Perspectives.DataTypeObjectGetters (buitenRol, contextType, getDisplayName, getRolBinding, getRolContext, rolType, getRolTypen, getRollen)
import Perspectives.TripleGetterConstructors (constructTripleGetterFromObjectsGetter)
import Prelude (pure)

identity :: forall e. ObjectsGetter e
identity x = pure [x]

-- | Identity for all values, contexts and roles.
-- | `forall a. a -> a`
identityM :: forall e. TypedTripleGetter e
identityM = constructTripleGetterFromObjectsGetter "model:Perspectives$identity" identity

-- | The type of the context instance.
-- | `psp:ContextInstance -> psp:Context`
contextTypeM :: forall e. TypedTripleGetter e
contextTypeM = constructTripleGetterFromObjectsGetter "model:Perspectives$type" contextType

-- | The type of the rol instance.
-- | `psp:RolInstance -> psp:Rol`
rolTypeM :: forall e. TypedTripleGetter e
rolTypeM = constructTripleGetterFromObjectsGetter "model:Perspectives$type" rolType

-- | `psp:ContextInstance -> psp:BuitenRol`
buitenRolM :: forall e. TypedTripleGetter e
buitenRolM = constructTripleGetterFromObjectsGetter "model:Perspectives$buitenRol" buitenRol

-- | Every rol instance belonging to the context instance.
-- | `psp:ContextInstance -> psp:RolInstance`
iedereRolInContext :: forall e. TypedTripleGetter e
iedereRolInContext =  constructTripleGetterFromObjectsGetter "model:Perspectives$iedereRolInContext" getRollen

-- | The types of the rol instances given to this context instance. Note: non-mandatory
-- | Rol types defined for the Context type may be missing!
-- | `psp:ContextInstance -> psp:Rol`
typeVanIedereRolInContext :: forall e. TypedTripleGetter e
typeVanIedereRolInContext =  constructTripleGetterFromObjectsGetter "model:Perspectives$typeVanIedereRolInContext" getRolTypen

-- | The rol instance that this rol instance is bound to, i.e. the head of its telescope.
-- | `psp:RolInstance -> psp:RolInstance`
binding :: forall e. TypedTripleGetter e
binding = constructTripleGetterFromObjectsGetter "model:Perspectives$binding" getRolBinding

-- | The context instance of the rol instance.
-- | `psp:RolInstance -> psp:ContextInstance`
context :: forall e. TypedTripleGetter e
context = constructTripleGetterFromObjectsGetter "model:Perspectives$context" getRolContext

-- | The string that labels the context instance.
-- | `psp:ContextInstance -> psp:String`
label :: forall e. TypedTripleGetter e
label = constructTripleGetterFromObjectsGetter "model:Perspectives$label" getDisplayName
