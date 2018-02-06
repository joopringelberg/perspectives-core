module Perspectives.ResourceTypes

where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar)
import Control.Monad.Eff (kind Effect)
import Data.Argonaut (Json)
import Data.StrMap (StrMap, keys)
import Network.HTTP.Affjax (AJAX)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap, new)
import Perspectives.Syntax (PerspectContext, PerspectRol)
import Prelude (class Show, show, unit, ($), (<>))

type Resource = String

type DomeinFileEffects e = (avar :: AVAR, ajax :: AJAX, gm :: GLOBALMAP | e)

type ResourceEffects e = DomeinFileEffects (prd :: PROPDEFS | e)

type AsyncResource e a = Aff (ResourceEffects e) a

foreign import data PROPDEFS :: Effect

-- | Type synonym AsyncDomeinFile is some type a in the extensible AsyncDomeinFileM monad.
type AsyncDomeinFile e a = Aff (DomeinFileEffects e) a

-- | A type for an untyped resource taken from Couchdb.
type CouchdbResource = StrMap Json

-- | A newtype for the property definitions so we can show them.
newtype PropDefs = PropDefs (StrMap Json)

instance showPropDefs :: Show PropDefs where
  show (PropDefs s) = "PropDefs=" <> (show $ keys s)

foreign import resource2json :: CouchdbResource -> Json

foreign import stringToRecord :: forall a. String -> {|a}

foreign import recordToJson :: forall a. {|a} -> Json

type RolDefinitions = GLStrMap (AVar PerspectRol)

type ContextDefinitions = GLStrMap (AVar PerspectContext)

rolDefinitions :: RolDefinitions
rolDefinitions = new unit

contextDefinitions :: ContextDefinitions
contextDefinitions = new unit
