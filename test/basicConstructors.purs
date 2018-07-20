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
import Perspectives.ContextRoleParser (ParseRoot(..), parseAndCache)
import Perspectives.CoreTypes (MonadPerspectives, (@@>))
import Perspectives.DataTypeObjectGetters (internePropertyTypen)
import Perspectives.DataTypeTripleGetters (contextTypeM)
import Perspectives.DomeinCache (storeDomeinFileInCouchdb)
import Perspectives.DomeinFile (DomeinFile(..))
import Perspectives.Effects (AjaxAvarCache)
import Perspectives.Identifiers (binnenRol)
import Perspectives.ModelBasedTripleGetters (internePropertiesDefM, ownInternePropertiesDefM)
import Perspectives.Resource (getPerspectEntiteit)
import Perspectives.RunMonadPerspectivesQuery ((##), (##=))
import Perspectives.SaveUserData (saveUserData)
import Perspectives.Syntax (PerspectContext)
import Perspectives.TypeDefChecker (checkModel)

test :: forall e. MonadPerspectives (AjaxAvarCache (console :: CONSOLE, fs :: FS, exception :: EXCEPTION | e)) Unit
test = do
  lift $ log "=========================CREATE A CONTEXT==================="
  cid <- pure "model:User$MijnEersteContext"
  typeId <- pure "model:CrlText$Text"
  r <- constructContext $
    ContextSerialization $ defaultContextSerializationRecord
      { id = "model:User$MijnEersteContext"
      , ctype = "model:CrlText$Text"
      , interneProperties = PropertySerialization $
          singleton "model:CrlText$Text$binnenRolBeschrijving$sourceText" ["Hello world!"]
      }
  lift $ log $ show r
