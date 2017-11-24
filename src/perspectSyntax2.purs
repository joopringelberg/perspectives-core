module Perspectives.Syntax2 where

import Data.Maybe (Maybe)
import Data.StrMap (StrMap, values)
import Prelude (class Show, show)

newtype ContextCollection = ContextCollection (StrMap PerspectEntity)

type ID = String

data PerspectEntity = Context PerspectContext | Rol PerspectRol

newtype PerspectContext = PerspectContext
  { id :: ID
  , pspType :: ID
  , binnenRol :: BinnenRol
  , buitenRol :: ID
  , rolInContext :: Array ID
  }

newtype PerspectRol =
  PerspectRol
    { id :: ID
    , pspType :: ID
    , binding :: Maybe ID
    , context :: ID
    , properties :: StrMap (Array String)
    , gevuldeRollen :: StrMap (Array ID)
    }

newtype BinnenRol =
  BinnenRol
    { id :: ID
    , pspType :: ID
    , binding :: Maybe ID
    , properties :: StrMap (Array String)
    }

-----------------------------------------------------------
-- Show instances
-----------------------------------------------------------

foreign import jsonStringify :: forall a. {|a} -> String

instance showPerspectContext :: Show PerspectContext where
  show (PerspectContext r) = jsonStringify r

instance showContextCollection :: Show ContextCollection where
  show (ContextCollection s) = show (values s)

instance showPerspectEntity :: Show PerspectEntity where
  show (Context ct) = show ct
  show (Rol r) = show r

instance showPerspectRol :: Show PerspectRol where
  show (PerspectRol r) = jsonStringify r
