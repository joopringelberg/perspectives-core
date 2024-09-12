module Perspectives.DataUpgrade.PatchModels.PDR2503 where

import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable)

foreign import simpleChat :: String 

replacements :: Object String
replacements = fromFoldable
  [ Tuple "model://perspectives.domains#SimpleChat" simpleChat
  
  ]