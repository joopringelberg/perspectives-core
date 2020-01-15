module Perspectives.External.CoreModules
(isExternalCoreModule)

where

import Data.Array (elemIndex)
import Data.Maybe (isJust)

coreModules :: Array String
coreModules =
  [ "model:Couchdb"
  ]

isExternalCoreModule :: String -> Boolean
isExternalCoreModule n = isJust (elemIndex n coreModules)
