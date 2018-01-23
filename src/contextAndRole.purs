module Perspectives.ContextAndRole where

import Perspectives.Syntax (BinnenRol(..), ID, PerspectContext(..), PerspectRol(..))

foreign import rolToContextID :: PerspectRol -> ID

foreign import contextToBuitenRolID :: PerspectContext -> ID

foreign import contextToBinnenRol :: PerspectContext -> BinnenRol
