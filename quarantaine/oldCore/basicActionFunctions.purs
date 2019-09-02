module Perspectives.BasicActionFunctions where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Perspectives.CoreTypes (MonadPerspectives, (##>>))
import Perspectives.DomeinCache (retrieveDomeinFileFromCache, storeDomeinFileInCouchdb)
import Perspectives.Representation.InstanceIdentifiers (ContextInstance, Value(..))

-- | Store the DomeinFile, if found in the cache.
storeDomeinFile :: ContextInstance -> MonadPerspectives Unit
storeDomeinFile textId = do
  syntacticState <- textId ##>> syntacticStateM
  case syntacticState of
    Value "false" -> pure unit
    otherwise -> do
      contextId <- textId ##>> parserMessagesM
      -- This must be the model name.
      -- here, because the syntactic state is true, there must be a contextId in the cache.
      mdf <- retrieveDomeinFileFromCache $ unwrap contextId
      case mdf of
        Nothing -> pure unit
        (Just df) -> storeDomeinFileInCouchdb df
-- TODO: zet een timestamp!
