module Perspectives.DataTypeTripleGetters where

import Perspectives.CoreTypes (ObjectsGetter, TypedTripleGetter)
import Perspectives.DataTypeObjectGetters (getBuitenRol, getContextType, getDisplayName, getRolBinding, getRolContext, getRolType, getRolTypen, getRollen)
import Perspectives.TripleGetterConstructors (constructTripleGetterFromObjectsGetter)
import Prelude (pure)

identity' :: forall e. ObjectsGetter e
identity' x = pure [x]

-- | Identity for all values, contexts and roles.
-- | `forall a. a -> a`
identity :: forall e. TypedTripleGetter e
identity = constructTripleGetterFromObjectsGetter "model:Perspectives$identity" identity'

-- | The type of the context instance.
-- | `psp:ContextInstance -> psp:Context`
contextType :: forall e. TypedTripleGetter e
contextType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getContextType

-- | The type of the rol instance.
-- | `psp:RolInstance -> psp:Rol`
rolType :: forall e. TypedTripleGetter e
rolType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getRolType

-- | `psp:ContextInstance -> psp:BuitenRol`
buitenRol :: forall e. TypedTripleGetter e
buitenRol = constructTripleGetterFromObjectsGetter "model:Perspectives$buitenRol" getBuitenRol

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
