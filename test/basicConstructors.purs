module Test.BasicConstructors where

import Prelude

import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.StrMap (singleton)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path as Path
import Perspectives.ApiTypes (ContextSerialization(..), PropertySerialization(..), defaultContextSerializationRecord)
import Perspectives.BasicConstructors (constructContext)
import Perspectives.CollectDomeinFile (domeinFileFromContext)
import Perspectives.ComputedTripleGetters (addComputedTripleGetters)
import Perspectives.ContextRoleParser (ParseRoot(..), parseAndCache)
import Perspectives.CoreTypes (MonadPerspectives, (@@>))
import Perspectives.DataTypeObjectGetters (internePropertyTypen)
import Perspectives.DataTypeTripleGetters (contextTypeM)
import Perspectives.DomeinCache (storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (binnenRol)
import Perspectives.ModelBasedTripleGetters (internePropertiesDefM, ownInternePropertiesDefM)
import Perspectives.QueryCompiler (propertyQuery)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery ((##), (##=))
import Perspectives.SaveUserData (saveUserData)
import Perspectives.Syntax (PerspectContext)
import Perspectives.TypeDefChecker (checkModel, getPropertyFunction)

modelDirectory :: String
modelDirectory = "/Users/joopringelberg/Code/perspectives-core/src/model"

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE, fs :: FS, exception :: EXCEPTION | e)) Unit
test = do
  lift $ log "=========================CREATE A CONTEXT==================="
  addComputedTripleGetters
  text <- lift $ liftEff $ readTextFile UTF8 (Path.concat [modelDirectory, "politie.crl"])
  r <- constructContext $
    ContextSerialization $ defaultContextSerializationRecord
      { id = "model:User$Politie_text"
      , ctype = "model:CrlText$Text"
      , interneProperties = PropertySerialization $
          singleton "model:CrlText$Text$binnenRolBeschrijving$sourceText" [text]
      }
  getParserMessages <- (getPropertyFunction "model:CrlText$Text$binnenRolBeschrijving$parserMessages" "model:CrlText$Text$binnenRolBeschrijving")
  parserMessages <- "model:User$Politie_text" ##= getParserMessages
  lift $ log $ show parserMessages
  getSyntacticState <- (getPropertyFunction "model:CrlText$Text$binnenRolBeschrijving$syntacticState" "model:CrlText$Text$binnenRolBeschrijving")
  parseState <-  "model:User$Politie_text" ##= getSyntacticState
  lift $ log $ show parseState
  getTypeCheckerMessages <- (getPropertyFunction "model:CrlText$Text$binnenRolBeschrijving$typeCheckerMessages" "model:CrlText$Text$binnenRolBeschrijving")
  typeCheckerMessages <-  "model:User$Politie_text" ##= getTypeCheckerMessages
  lift $ log $ show typeCheckerMessages
