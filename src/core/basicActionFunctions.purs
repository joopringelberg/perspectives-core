module Perspectives.BasicActionFunctions where

import Prelude

import Data.Maybe (Maybe(..))
import Perspectives.ComputedTripleGetters (parserMessagesM, syntacticStateM)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (retrieveDomeinFileFromCache, storeDomeinFileInCouchdb)
import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.RunMonadPerspectivesQuery ((##>>))

-- | Store the DomeinFile, if found in the cache.
storeDomeinFile :: ID -> MonadPerspectives Unit
storeDomeinFile textId = do
  syntacticState <- textId ##>> syntacticStateM
  case syntacticState of
    "false" -> pure unit
    otherwise -> do
      contextId <- textId ##>> parserMessagesM
      -- This must be the model name.
      -- here, because the syntactic state is true, there must be a contextId in the cache.
      mdf <- retrieveDomeinFileFromCache contextId
      case mdf of
        Nothing -> pure unit
        (Just df) -> storeDomeinFileInCouchdb df
-- TODO: zet een timestamp!
