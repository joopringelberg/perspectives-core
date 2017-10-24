module Perspectives.ResourceTypes
( AsyncResourceM
, AsyncResource
, AsyncDomeinFileM
, AsyncDomeinFile
, PropDefs(..)
, Resource
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

type AsyncResourceM e = Aff (avar :: AVAR, ajax :: AJAX | e)
type AsyncResource e a = AsyncResourceM e a

-- | AsyncDomeinFileM is a type synonym for a monad with extensible effects including at least GLOBALMAP,
-- | AVAR and AJAX.
type AsyncDomeinFileM e =  Aff (gm :: GLOBALMAP, avar :: AVAR, ajax :: AJAX | e)

-- | Type synonym AsyncDomeinFile is some type a in the extensible AsyncDomeinFileM monad.
type AsyncDomeinFile e a = AsyncDomeinFileM e a

-- | A newtype for the property definitions so we can show them.
newtype PropDefs = PropDefs (StrMap Json)

instance showPropDefs :: Show PropDefs where
  show (PropDefs s) = "PropDefs=" <> (show $ keys s)
