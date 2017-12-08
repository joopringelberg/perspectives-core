module Perspectives.ResourceTypes
( AsyncResource
, ResourceEffects
, DomeinFileEffects
, AsyncDomeinFile
, PropDefs(..)
, Resource
, CouchdbResource
)

where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Data.Argonaut (Json)
import Data.StrMap (StrMap, keys)
import Network.HTTP.Affjax (AJAX)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Prelude (class Show, show, ($), (<>))

type Resource = String

type ResourceEffects e = (avar :: AVAR, ajax :: AJAX | e)

type AsyncResource e a = Aff (ResourceEffects e) a

type DomeinFileEffects e = ResourceEffects (gm :: GLOBALMAP | e)

-- | Type synonym AsyncDomeinFile is some type a in the extensible AsyncDomeinFileM monad.
type AsyncDomeinFile e a = Aff (DomeinFileEffects e) a

-- | A type for an untyped resource taken from Couchdb.
type CouchdbResource = StrMap Json

-- | A newtype for the property definitions so we can show them.
newtype PropDefs = PropDefs (StrMap Json)

instance showPropDefs :: Show PropDefs where
  show (PropDefs s) = "PropDefs=" <> (show $ keys s)
