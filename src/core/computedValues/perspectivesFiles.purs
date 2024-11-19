module Perspectives.Extern.Files where

import Prelude

import Control.Monad.Error.Class (try)
import Control.Monad.Trans.Class (lift)
import Data.Array (head, elemIndex)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..))
import Foreign (Foreign)
import Perspectives.CoreTypes (MonadPerspectivesQuery, MonadPerspectives)
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
  (ArrayT $ case head fileInfo_ of
    Just v -> do 
      mText <- lift $ getPFileTextValue v
      case mText of 
        Nothing -> pure []
        Just text -> pure [text]
    Nothing -> pure [])
  >>= handleExternalFunctionError "model://perspectives.domains#Files$FileText"

getPFileTextValue :: String -> MonadPerspectives (Maybe String)
getPFileTextValue v = case parsePerspectivesFile v of
  Left e -> pure Nothing
  Right {propertyType, roleFileName, database, mimeType} -> if isATextType mimeType
    then do
      db <- case database of 
        Just db -> pure db
        Nothing -> entitiesDatabaseName
      (ma :: Maybe Foreign) <- getAttachment db roleFileName (typeUri2couchdbFilename $ unwrap propertyType)
      case ma of
        Nothing -> pure Nothing
        Just a -> Just <$> (lift $ fromBlob a)
    else pure Nothing
  where
    isATextType :: String -> Boolean
    isATextType s = (isJust $ match (unsafeRegex "^text/" noFlags) s) || 
      (isJust $ elemIndex s ["application/x-yaml"])

externalFunctions :: Array (Tuple String HiddenFunctionDescription)
externalFunctions =
  [ Tuple "model://perspectives.domains#Files$FileText" {func: unsafeCoerce fileText, nArgs: 1, isFunctional: True, isEffect: false}
  ]
