module Test.BasicConstructors where

import Prelude

import Control.Monad.Trans.Class (lift)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign.Generic (encodeJSON)
import Foreign.Object (singleton)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), defaultContextSerializationRecord)
import Perspectives.BasicConstructors (constructContext)
import Perspectives.ComputedTripleGetters (addComputedTripleGetters)
import Perspectives.CoreTypes (MonadPerspectives)
import Perspectives.QueryCompiler (getPropertyFunction)
import Perspectives.RunMonadPerspectivesQuery ((##=))

modelDirectory :: String
modelDirectory = "/Users/joopringelberg/Code/perspectives-core/src/model"

test :: MonadPerspectives Unit
test = do
  lift $ log "=========================CREATE A CONTEXT==================="
  addComputedTripleGetters
  text <- lift $ liftEffect $ readTextFile UTF8 (Path.concat [modelDirectory, "politie.crl"])
  s <- pure $ ContextSerialization $ defaultContextSerializationRecord
    { id = "model:User$Politie_text"
    , ctype = "model:CrlText$Text"
    , interneProperties = PropertySerialization $
        singleton "model:CrlText$Text$binnenRolBeschrijving$sourceText" [text]
    }
  lift $ log $ encodeJSON s
  r <- constructContext s
  lift $ log "=========================PARSE A TEXT==================="
  getParserMessages <- (getPropertyFunction "model:CrlText$Text$binnenRolBeschrijving$parserMessages")
  parserMessages <- "model:User$Politie_text" ##= getParserMessages
  lift $ log $ show parserMessages
  lift $ log "=========================SYNTACTIC STATE==================="
  getSyntacticState <- (getPropertyFunction "model:CrlText$Text$binnenRolBeschrijving$syntacticState")
  parseState <-  "model:User$Politie_text" ##= getSyntacticState
  lift $ log $ show parseState
  lift $ log "=========================TYPE CHECKER MESSAGES==================="
  getTypeCheckerMessages <- (getPropertyFunction "model:CrlText$Text$binnenRolBeschrijving$typeCheckerMessages")
  typeCheckerMessages <-  "model:User$Politie_text" ##= getTypeCheckerMessages
  lift $ log $ show typeCheckerMessages
  lift $ log "=========================SEMANTIC STATE==================="
  getSemanticState <- (getPropertyFunction "model:CrlText$Text$binnenRolBeschrijving$semanticState")
  semanticState <-  "model:User$Politie_text" ##= getSemanticState
  lift $ log $ show semanticState
