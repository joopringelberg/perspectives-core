module Perspectives.Syntax2 where

import Data.Maybe (Maybe)
import Data.StrMap (StrMap)

newtype ContextCollection = ContextCollection (StrMap PerspectEntity)

type ID = String

data PerspectEntity = Context PerspectContext | Rol PerspectRol

newtype PerspectContext = PerspectContext
  { id :: ID
  , pspType :: ID
  , binnenRol :: PerspectRol
  , buitenRol :: ID
  , rolInContext :: StrMap (Array ID)
  , gevuldeRollen :: StrMap (Array ID)
  }

newtype PerspectRol =
  PerspectRol
    { id :: ID
    , pspType :: ID
    , binding :: Maybe ID
    , context :: PerspectContext
    , properties :: StrMap (Array String)
    , gevuldeRollen :: StrMap (Array ID)
    }
