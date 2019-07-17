module Perspectives.BasicActionFunctions where

import Prelude

-- import Perspectives.ComputedTripleGetters (parserMessagesM, syntacticStateM)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.DomeinCache (storeDomeinFileInCouchdb)

import Perspectives.EntiteitAndRDFAliases (ID)
import Perspectives.Instances (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery ((##>>))

storeDomeinFile :: ID -> MonadPerspectives Unit
storeDomeinFile textId = do
  syntacticState <- textId ##>> syntacticStateM
  case syntacticState of
    "false" -> pure unit
    otherwise -> do
      contextId <- textId ##>> parserMessagesM
      -- here, because the syntactic state is true, there must be a contextId in the cache.
      ctxt <- getPerspectEntiteit contextId
      df <- domeinFileFromContext ctxt
      storeDomeinFileInCouchdb df
-- TODO: zet een timestamp!
