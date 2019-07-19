module Perspectives.Representation.Types.TripleGetters where

import Data.Newtype (class Newtype)
import Perspectives.CoreTypes (TypedTripleGetter)
import Perspectives.Representation.Class.Role (class RoleClass)
import Perspectives.Representation.Class.Role (properties) as Role
import Perspectives.Representation.TypeIdentifiers (PropertyType)
import Perspectives.TripleGetters.TrackedAs (trackedAs)

properties :: forall r. Newtype r String => RoleClass r => TypedTripleGetter r PropertyType
properties = Role.properties `trackedAs` "properties"
