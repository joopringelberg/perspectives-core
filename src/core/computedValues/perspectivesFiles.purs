module Perspectives.Extern.Files where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, singleton)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Perspectives.CoreTypes (MonadPerspectivesQuery)
import Perspectives.DependencyTracking.Array.Trans (ArrayT(..))
import Perspectives.Error.Boundaries (handleExternalFunctionError)
import Perspectives.External.HiddenFunctionCache (HiddenFunctionDescription)
import Perspectives.Identifiers (typeUri2couchdbFilename)
import Perspectives.Instances.Values (parsePerspectivesFile)
import Perspectives.Persistence.API (fromBlob, getAttachment)
import Perspectives.Persistent (entitiesDatabaseName)
import Perspectives.Representation.ThreeValuedLogic (ThreeValuedLogic(..))
import Unsafe.Coerce (unsafeCoerce)

fileText :: Array String -> String -> MonadPerspectivesQuery String
fileText fileInfo_ _ = try
  (ArrayT case head fileInfo_ of
    Just v -> case parsePerspectivesFile v of
      Left e -> pure []
      Right {propertyType, roleFileName, database, mimeType} -> if isATextType mimeType
        then case database of 
          Just db -> do
            (ma :: Maybe Foreign) <- lift $ getAttachment db roleFileName (typeUri2couchdbFilename $ unwrap propertyType)
            case ma of
              Nothing -> pure []
              Just a -> (lift $ lift $ fromBlob a) >>= pure <<< singleton
          Nothing -> do 
            db <- lift $ entitiesDatabaseName
            (ma :: Maybe Foreign) <- lift $ getAttachment db roleFileName (typeUri2couchdbFilename $ unwrap propertyType)
            case ma of
              Nothing -> pure []
              Just a -> (lift $ lift $ fromBlob a) >>= pure <<< singleton
        else pure []
    Nothing -> pure [])
  >>= handleExternalFunctionError "model://perspectives.domains#Files$FileText"

  where
    isATextType :: String -> Boolean
    isATextType = isJust <<< match (unsafeRegex "^text/" noFlags)

externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#Files$FileText" {func: unsafeCoerce fileText, nArgs: 1, isFunctional: True}
  ]
