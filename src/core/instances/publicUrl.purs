module Perspectives.InstanceRepresentation.PublicUrl where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Simple.JSON (class ReadForeign, class WriteForeign, read', writeImpl)

data PublicUrl = NONE | URL String

derive instance Generic PublicUrl _

instance Show PublicUrl where show = genericShow

instance WriteForeign PublicUrl where
  writeImpl NONE = writeImpl {url: "none"}
  writeImpl (URL s) = writeImpl {url: s}

instance ReadForeign PublicUrl where
  readImpl f = do
    {url} :: {url :: String} <- read' f
    case url of 
      "none" -> pure NONE
      _ -> pure $ URL url


instance Eq PublicUrl where 
  eq NONE NONE = true
  eq (URL s1) (URL s2) = eq s1 s2
  eq _ _ = false

