module Perspectives.External.CoreModules
( isExternalCoreModule
, addAllExternalFunctions
, addExternalFunctionForModule
)

where

import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Foreign.Object (Object, fromFoldable, lookup)
import Perspectives.Extern.Couchdb (externalFunctions) as ExternalCouchdb
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription, hiddenFunctionInsert)
import Perspectives.Instances.SerialiseAsJson (externalFunctions) as Serialise
import Prelude (class Monad, Unit, discard, pure, unit, ($))

type ExternalFunctions = Array (Tuple String HiddenFunctionDescription)

coreModules :: Object ExternalFunctions
coreModules = fromFoldable
  [ Tuple "model:Couchdb" ExternalCouchdb.externalFunctions
  , Tuple "model:Serialise" Serialise.externalFunctions
  ]

isExternalCoreModule :: String -> Boolean
isExternalCoreModule n = isJust (lookup n coreModules)

addAllExternalFunctions :: forall m. Monad m => m Unit
addAllExternalFunctions = do
  addExternalFunctions ExternalCouchdb.externalFunctions
  addExternalFunctions Serialise.externalFunctions

addExternalFunctions :: forall m. Monad m => Array (Tuple String HiddenFunctionDescription) -> m Unit
addExternalFunctions externalFunctions = for_ externalFunctions \(Tuple n f) -> pure $ hiddenFunctionInsert n f.func f.nArgs

addExternalFunctionForModule :: forall m. Monad m => String -> m Unit
addExternalFunctionForModule moduleName =
  case lookup moduleName coreModules of
    Nothing -> pure unit
    Just functions -> addExternalFunctions functions
