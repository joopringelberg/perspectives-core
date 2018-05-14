module Perspectives.DataTypeTripleGetters where

import Data.Array (elemIndex)
import Data.Maybe (maybe)
import Perspectives.CoreTypes (ObjectsGetter, TypedTripleGetter)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Property (getBuitenRol, getContextType, getDisplayName, getRolBinding, getRolContext, getRolType, getRolTypen, getRollen)
import Perspectives.TripleGetterConstructors (constructTripleGetterFromObjectsGetter)
import Prelude (const, pure, (<>), (>=>))

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
rolTypen :: forall e. TypedTripleGetter e
rolTypen =  constructTripleGetterFromObjectsGetter "model:Perspectives$rolTypen" getRolTypen

-- | The type of the rol instance.
-- | `psp:RolInstance -> psp:Rol`
rolType :: forall e. TypedTripleGetter e
rolType = constructTripleGetterFromObjectsGetter "model:Perspectives$type" getRolType

-- | The rol instance that this rol instance is bound to, i.e. the head of its telescope.
-- | `psp:RolInstance -> psp:RolInstance`
binding :: forall e. TypedTripleGetter e
binding = constructTripleGetterFromObjectsGetter "model:Perspectives$binding" getRolBinding

-- | The context instance of the rol instance.
-- | `psp:RolInstance -> psp:ContextInstance`
rolContext :: forall e. TypedTripleGetter e
rolContext = constructTripleGetterFromObjectsGetter "model:Perspectives$context" getRolContext

-- | The string that labels the context instance.
-- | `psp:ContextInstance -> psp:String`
label :: forall e. TypedTripleGetter e
label = constructTripleGetterFromObjectsGetter "model:Perspectives$label" getDisplayName

-- | A combinator from the type name of a Rol to a query that takes the instance of a Rol
-- | and returns a boolean value showing if the instance has the given type.
-- | NOTE: makes no use of Aspects!
-- | `psp:Rol -> psp:RolInstance -> psp:Boolean`
rolHasType :: forall e. ID -> TypedTripleGetter e
rolHasType typeId = constructTripleGetterFromObjectsGetter ("model:Perspectives$rolHasType" <> "_" <> typeId)
  (getRolType >=> \(objs::Array String) -> pure (maybe ["false"] (const ["true"]) (elemIndex typeId objs)))
