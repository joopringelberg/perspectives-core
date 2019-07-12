module Perspectives.Representation.Class.Revision where

import Data.Maybe (Maybe)
import Perspectives.InstanceRepresentation (Revision) as B

class Revision v where
  rev :: v -> B.Revision
  changeRevision :: Maybe String -> v -> v
