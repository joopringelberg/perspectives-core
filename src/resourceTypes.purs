module Perspectives.ResourceTypes

where

import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Eff (kind Effect)
import Data.Argonaut (Json)
import Data.StrMap (StrMap, keys)
import Perspectives.GlobalUnsafeStrMap (GLStrMap, new)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (class Show, show, unit, ($), (<>))

type Resource = String

-- | A type for an untyped resource taken from Couchdb.
type CouchdbResource = StrMap Json

-- | A newtype for the property definitions so we can show them.
newtype PropDefs = PropDefs (StrMap Json)

instance showPropDefs :: Show PropDefs where
  show (PropDefs s) = "PropDefs=" <> (show $ keys s)

type RolDefinitions = GLStrMap (AVar PerspectRol)

type ContextDefinitions = GLStrMap (AVar PerspectContext)

rolDefinitions :: RolDefinitions
rolDefinitions = new unit

contextDefinitions :: ContextDefinitions
contextDefinitions = new unit

-- TODO: alleen gebruikt in createResourceInCouchdb van module Perspectives.ResourceRetrieval.
foreign import stringToRecord :: forall a. String -> {|a}
