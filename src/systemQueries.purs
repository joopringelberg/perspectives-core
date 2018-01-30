module Perspectives.SystemQueries where

import Perspectives.Property (ObjectsGetter, getBuitenRol, getContextType, getRolBinding, getRolContext, getRolType, getRolTypen, getRollen)
import Perspectives.QueryCombinators (hasValue) as QC
import Perspectives.TripleGetter (NamedTripleGetter, constructPublicPropertyGetter, constructTripleGetterFromArbitraryFunction)
import Prelude (pure)

identity' :: forall e. ObjectsGetter e
identity' id = pure [id]

identity :: forall e. NamedTripleGetter e
identity = constructTripleGetterFromArbitraryFunction "identity" identity'

contextType :: forall e. NamedTripleGetter e
contextType = constructTripleGetterFromArbitraryFunction "psp:type" getContextType

buitenRol :: forall e. NamedTripleGetter e
buitenRol = constructTripleGetterFromArbitraryFunction "psp:buitenRol" getBuitenRol

rollen :: forall e. NamedTripleGetter e
rollen =  constructTripleGetterFromArbitraryFunction "psp:rollen" getRollen

rolTypen :: forall e. NamedTripleGetter e
rolTypen =  constructTripleGetterFromArbitraryFunction "psp:rolTypen" getRolTypen

rolType :: forall e. NamedTripleGetter e
rolType = constructTripleGetterFromArbitraryFunction "psp:type" getRolType

binding :: forall e. NamedTripleGetter e
binding = constructTripleGetterFromArbitraryFunction "psp:binding" getRolBinding

rolContext :: forall e. NamedTripleGetter e
rolContext = constructTripleGetterFromArbitraryFunction "psp:context" getRolContext

isFunctional :: forall e. NamedTripleGetter e
isFunctional = constructPublicPropertyGetter "psp:isFunctional"

isVerplicht :: forall e. NamedTripleGetter e
isVerplicht = constructPublicPropertyGetter "psp:isVerplicht"

range :: forall e. NamedTripleGetter e
range = constructPublicPropertyGetter "psp:range"

label :: forall e. NamedTripleGetter e
label = constructPublicPropertyGetter "psp:label"

hasLabel :: forall e. NamedTripleGetter e
hasLabel = QC.hasValue label

hasBinding :: forall e. NamedTripleGetter e
hasBinding = QC.hasValue binding
