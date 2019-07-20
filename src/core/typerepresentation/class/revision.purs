module Perspectives.Representation.Class.Revision where

import Data.Maybe (Maybe(..))

class Revision v where
  rev :: v -> Revision_
  changeRevision :: Maybe String -> v -> v

type Revision_ = Maybe String

revision :: String -> Revision_
revision = Just
