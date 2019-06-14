module Perspectives.DataTypeTripleGetters where

import Perspectives.ApiTypes (ContextID)
import Perspectives.CoreTypes (type (**>), type (~~>))
import Perspectives.DataTypeObjectGetters (binnenRol, buitenRol, context, contextType, genericContext, iedereRolInContext, label, propertyTypen, rolType, typeVanIedereRolInContext, genericRolType) as DTOG
import Perspectives.EntiteitAndRDFAliases (RolName)
import Perspectives.Identifiers (LocalName)
import Perspectives.PerspectivesTypes (class Binding, class RolClass, AnyContext, AnyDefinition, BinnenRol, BuitenRol, RolDef, Value)
import Perspectives.PerspectivesTypes (genericBinding, binding, getUnqualifiedProperty) as PT
import Perspectives.TripleGetterComposition ((>->))
import Perspectives.TripleGetterFromObjectGetter (trackedAs)
import Prelude (class Show, pure)

identity_ :: forall o.  Show o => (o ~~> o)
identity_ x = pure [x]

-- | Identity for all values, contexts and roles.
identity :: forall o.  Show o => (o **> o)
identity = identity_ `trackedAs` "model:Perspectives$identity"

-- | The type of the context instance.
contextType :: (AnyContext **> AnyDefinition)
contextType = DTOG.contextType `trackedAs` "model:Perspectives$type"

buitenRol :: (AnyContext **> BuitenRol)
buitenRol = DTOG.buitenRol `trackedAs` "model:Perspectives$buitenRol"

-- buitenRol'

binnenRol :: (AnyContext **> BinnenRol)
binnenRol = DTOG.binnenRol `trackedAs` "model:Perspectives$binnenRol"

-- | Every rol instance belonging to the context instance.
iedereRolInContext :: (String **> String)
iedereRolInContext = DTOG.iedereRolInContext `trackedAs`  "model:Perspectives$iedereRolInContext"

-- | The types of the rol instances given to this context instance. Note: non-mandatory
-- | Rol types defined for the Context type may be missing!
typeVanIedereRolInContext :: (String **> String)
typeVanIedereRolInContext =  DTOG.typeVanIedereRolInContext `trackedAs` "model:Perspectives$typeVanIedereRolInContext"

-- | The types of every property for which this rol has a value.
propertyTypen :: (String **> String)
propertyTypen = DTOG.propertyTypen `trackedAs` "model:Perspectives$typeVanIederePropertyVanRol"

-- | The string that labels the context instance.
label :: (AnyContext **> String)
label = DTOG.label `trackedAs` "model:Perspectives$label"

-- | The type of the rol instance.
rolType :: forall r. RolClass r => (r **> RolDef)
rolType = DTOG.rolType `trackedAs` "model:Perspectives$type"

genericRolType :: (String **> String)
genericRolType = DTOG.genericRolType `trackedAs` "model:Perspectives$type"

-- | The rol instance that this rol instance is bound to, i.e. the head of its telescope.
-- | `psp:RolInstance -> psp:RolInstance`
binding :: forall binder bound. Show binder => Binding binder bound => (binder **> bound)
binding = PT.binding `trackedAs` "model:Perspectives$binding"

rolBindingDef :: forall r b. Show r => Binding r b => (r **> AnyContext)
rolBindingDef = binding >-> context

-- | The context instance of the rol instance.
-- | `psp:RolInstance -> psp:ContextInstance`
context :: forall r. RolClass r => (r **> String)
context = DTOG.context `trackedAs` "model:Perspectives$context"

genericContext :: (RolName **> ContextID)
genericContext = DTOG.genericContext `trackedAs` "model:Perspectives$context"

genericBinding :: (RolName **> RolName)
genericBinding = PT.genericBinding `trackedAs` "model:Perspectives$binding"

genericRolBindingDef :: (RolName **> AnyContext)
genericRolBindingDef = genericBinding >-> genericContext

getUnqualifiedProperty :: forall r. RolClass r => LocalName -> (r **> Value)
getUnqualifiedProperty ln = (PT.getUnqualifiedProperty ln) `trackedAs` ln
