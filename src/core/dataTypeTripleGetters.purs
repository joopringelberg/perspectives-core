module Perspectives.DataTypeTripleGetters where

import Perspectives.CoreTypes (ObjectsGetter, type (**>))
import Perspectives.DataTypeObjectGetters (binnenRol, buitenRol, context, contextType, genericContext, iedereRolInContext, internePropertyTypen, label, propertyTypen, rolType, typeVanIedereRolInContext, genericRolType) as DTOG
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, AnyDefinition, BinnenRol, BuitenRol, RolDef, typeWithPerspectivesTypes)
import Perspectives.PerspectivesTypes (genericBinding, binding) as PT
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterFromObjectGetter (constructTripleGetterFromObjectsGetter, trackedAs)
import Prelude (pure)

identity_ :: forall e. ObjectsGetter e
identity_ x = pure [x]

-- | Identity for all values, contexts and roles.
identity :: forall o e. (o **> o) e
identity = typeWithPerspectivesTypes constructTripleGetterFromObjectsGetter "model:Perspectives$identity" identity_

-- | The type of the context instance.
contextType :: forall e. (AnyContext **> AnyDefinition) e
contextType = DTOG.contextType `trackedAs` "model:Perspectives$type"

buitenRol :: forall e. (AnyContext **> BuitenRol) e
buitenRol = DTOG.buitenRol `trackedAs` "model:Perspectives$buitenRol"

-- buitenRol'

binnenRol :: forall e. (AnyContext **> BinnenRol) e
binnenRol = DTOG.binnenRol `trackedAs` "model:Perspectives$binnenRol"

-- | Every rol instance belonging to the context instance.
iedereRolInContext :: forall e. (String **> String) e
iedereRolInContext = DTOG.iedereRolInContext `trackedAs`  "model:Perspectives$iedereRolInContext"

-- | The types of the rol instances given to this context instance. Note: non-mandatory
-- | Rol types defined for the Context type may be missing!
typeVanIedereRolInContext :: forall e. (String **> String) e
typeVanIedereRolInContext =  DTOG.typeVanIedereRolInContext `trackedAs` "model:Perspectives$typeVanIedereRolInContext"

-- | The types of every property for which this rol has a value.
propertyTypen :: forall e. (String **> String) e
propertyTypen = DTOG.propertyTypen `trackedAs` "model:Perspectives$typeVanIederePropertyVanRol"

internePropertyTypen :: forall e. (String **> String) e
internePropertyTypen = DTOG.internePropertyTypen `trackedAs` "model:Perspectives$typeVanIederePropertyVanBinnenRol"

-- | The string that labels the context instance.
label :: forall e. (AnyContext **> String) e
label = DTOG.label `trackedAs` "model:Perspectives$label"

-- | The type of the rol instance.
rolType :: forall r e. RolClass r => (r **> RolDef) e
rolType = DTOG.rolType `trackedAs` "model:Perspectives$type"

genericRolType :: forall e. (String **> String) e
genericRolType = DTOG.genericRolType `trackedAs` "model:Perspectives$type"

-- | The rol instance that this rol instance is bound to, i.e. the head of its telescope.
-- | `psp:RolInstance -> psp:RolInstance`
binding :: forall binder bound e. Binding binder bound => (binder **> bound) e
binding = PT.binding `trackedAs` "model:Perspectives$binding"

rolBindingDef :: forall r b e. Binding r b => (r **> AnyContext) e
rolBindingDef = binding >-> context

-- | The context instance of the rol instance.
-- | `psp:RolInstance -> psp:ContextInstance`
context :: forall r e. RolClass r => (r **> String) e
context = DTOG.context `trackedAs` "model:Perspectives$context"

genericContext :: forall e. (String **> String) e
genericContext = DTOG.genericContext `trackedAs` "model:Perspectives$context"

genericBinding :: forall e. (String **> String) e
genericBinding = PT.genericBinding `trackedAs` "model:Perspectives$binding"
