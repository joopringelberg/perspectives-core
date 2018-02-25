module Test.Couchdb where

import Prelude
import Perspectives.Couchdb.Databases (createDatabase, requestAuthentication)
-----------------------------------------------------------
-- Tests
-----------------------------------------------------------

-- runAff_ (\_->pure unit) ((runIndentParser test1 context) >>= (\r -> log (show r)))

-- test = runTest $ runPerspectives "admin" "admin" authenticate
test = do
  requestAuthentication
  createDatabase "test1"
