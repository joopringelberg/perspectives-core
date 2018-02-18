module Perspectives.EntiteitCache

where

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Eff (kind Effect)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (unit)

type RolDefinitions = GLStrMap (AVar PerspectRol)

type ContextDefinitions = GLStrMap (AVar PerspectContext)

rolDefinitions :: RolDefinitions
rolDefinitions = new unit

contextDefinitions :: ContextDefinitions
contextDefinitions = new unit
