module Perspectives.Instances.TripleGetters where

import Data.Array (singleton)
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (StringTypedTripleGetter)
import Perspectives.Instances.ObjectGetters (context, binding, roleType, contextType) as OG
import Perspectives.TripleGetters.TrackedAs (trackedAs)
import Prelude ((>=>), (<<<), pure)

context :: StringTypedTripleGetter
context = OG.context `trackedAs` "context"

binding :: StringTypedTripleGetter
binding = OG.binding `trackedAs` "binding"

roleType :: StringTypedTripleGetter
roleType = (OG.roleType >=> pure <<< singleton <<< unwrap) `trackedAs` "roleType"

contextType :: StringTypedTripleGetter
contextType = (OG.contextType >=> pure <<< singleton <<< unwrap) `trackedAs` "contextType"
