module Perspectives.DataUpgrade.PatchModels.PDR2505 where

import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable)

foreign import couchdbManagement :: String 

replacements :: Object String
replacements = fromFoldable
  [ Tuple "model://perspectives.domains#CouchdbManagement" couchdbManagement
  
  ] 