module Perspectives.ResourceTypes
( ResourceId
, AsyncResource
, AsyncDomeinFile
, PropDefs(..)
, Resource(..)
, ResourceLocation(..))

where

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR, AVar)
import Data.Argonaut (Json)
import Data.Eq (class Eq)
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, keys)
import Network.HTTP.Affjax (AJAX)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP)
import Perspectives.Location (Location)
import Prelude (class Show, eq, show, ($), (<>))

type ResourceId = String

type AsyncResource e a = Aff (avar :: AVAR, ajax :: AJAX | e) a

type AsyncDomeinFile e a = Aff (gm :: GLOBALMAP, avar :: AVAR, ajax :: AJAX | e) a

-- | A newtype for the property definitions so we can show them.
newtype PropDefs = PropDefs (StrMap Json)

instance showPropDefs :: Show PropDefs where
  show (PropDefs s) = show $ keys s

-- | Basic representation for Resource, complete with its definition.
newtype Resource = Resource
  { id :: String
  , propDefs :: Maybe (AVar PropDefs)
  }

-- | The show instance cannot inspect the content of the AVar, because that needs running
-- | the asynchronous computation to yield a result synchronously, which we cannot do.
instance showResource :: Show Resource where
    show (Resource{id, propDefs} ) = case propDefs of
      Nothing -> id <> " (No definition)"
      (Just avar) -> id <> " (with definition)"

instance eqResource :: Eq Resource where
  eq (Resource{id : id1}) (Resource{id : id2}) = eq id1 id2

-- | We store the combination of Resource and Location in an Index.
newtype ResourceLocation = ResourceLocation{ res :: Resource, loc :: Location Resource}

--instance showResourceLocation :: Show (ResourceLocation e) where
--  show (ResourceLocation{ res }) = show res
