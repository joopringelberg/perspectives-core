module Perspectives.Instances.TripleGetters where

import Data.Array (singleton)
import Perspectives.CoreTypes (type (**>))
import Perspectives.Instances.ObjectGetters (context, binding, roleType, contextType) as OG
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, RoleInstance)
import Perspectives.Representation.TypeIdentifiers (ContextType, EnumeratedRoleType)
import Perspectives.TripleGetters.TrackedAs (trackedAs)
import Prelude ((>=>), (<<<), pure)

context :: RoleInstance **> ContextInstance
context = OG.context `trackedAs` "context"

binding :: RoleInstance **> RoleInstance
binding = OG.binding `trackedAs` "binding"

roleType :: RoleInstance **> EnumeratedRoleType
roleType = (OG.roleType >=> pure <<< singleton) `trackedAs` "roleType"

contextType :: ContextInstance **> ContextType
contextType = (OG.contextType >=> pure <<< singleton) `trackedAs` "contextType"
