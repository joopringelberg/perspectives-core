module Test.Couchdb where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.AVar (AVAR, AVar)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Reader (ReaderT)
import Network.HTTP.Affjax (AJAX)
import Perspectives.Couchdb.Databases (createDatabase, requestAuthentication)
import Perspectives.DomeinFile (DomeinFile)
import Perspectives.GlobalUnsafeStrMap (GLOBALMAP, GLStrMap)
import Perspectives.Syntax (PerspectContext, PerspectRol)
-----------------------------------------------------------
-- Tests
-----------------------------------------------------------

-- runAff_ (\_->pure unit) ((runIndentParser test1 context) >>= (\r -> log (show r)))

-- test = runTest $ runPerspectives "admin" "admin" authenticate
test :: forall t5.
      ReaderT
        (AVar
           { rolDefinitions :: GLStrMap (AVar PerspectRol)
           , contextDefinitions :: GLStrMap (AVar PerspectContext)
           , domeinCache :: GLStrMap (AVar DomeinFile)
           , userInfo :: { userName :: String
                         , couchdbPassword :: String
                         , couchdbBaseURL :: String
                         }
           , couchdbSessionStarted :: Boolean
           , sessionCookie :: AVar String
           , memorizeQueryResults :: Boolean
           }
        )
        (Aff
           ( ref :: REF
           , avar :: AVAR
           , ajax :: AJAX
           , gm :: GLOBALMAP
           | t5
           )
        )
        Unit
test = do
  requestAuthentication
  createDatabase "test1"
