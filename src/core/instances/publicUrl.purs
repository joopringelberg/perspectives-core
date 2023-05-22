module Perspectives.InstanceRepresentation.PublicUrl where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Foreign.Generic (class Decode, class Encode, defaultOptions, genericDecode, genericEncode)

data PublicUrl = NONE | URL String

derive instance Generic PublicUrl _

instance Show PublicUrl where show = genericShow

instance Encode PublicUrl where encode = genericEncode defaultOptions

instance Decode PublicUrl where decode = genericDecode defaultOptions

instance Eq PublicUrl where 
  eq NONE NONE = true
  eq (URL s1) (URL s2) = eq s1 s2
  eq _ _ = false

