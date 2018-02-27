module Test.Couchdb where

import Control.Monad.Aff (Aff)
import Data.Argonaut (fromString)
import Data.Argonaut.Core (fromObject)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.StrMap (fromFoldable) as StrMap
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX, post, put, AffjaxResponse)
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Prelude (Unit, bind, pure, unit, ($), (<>))

-- Run this function on a couchdb 2.1.1 instance with CORS set up or from the localhost.
-- Puts a new admin with the given user name and password.
-- Then creates a session.
-- Finally tries to create a database.
-- No analysis on what is returned from the server for clarities sake.
-- This code fails, because the database cannot be created because: "You are not a server admin.".
test :: forall e. String -> String -> String -> Aff (ajax :: AJAX | e) Unit
test usr pwd dbName = do
  (_ :: AffjaxResponse String) <- put ("http://127.0.0.1:5984/_node/couchdb@localhost/_config/admins/" <> usr) (fromString pwd)
  (_ :: AffjaxResponse PostCouchdb_session) <- post "http://127.0.0.1:5984/_session"
    (fromObject (StrMap.fromFoldable [Tuple "name" (fromString usr), Tuple "password" (fromString pwd)]))
  -- the argument to post is in essence {"name": <usr>, "password": <pwd>}
  (_ :: AffjaxResponse String) <-put ("http://127.0.0.1:5984/" <> dbName) ""
  pure unit

-- Just a definition for the return value of post.
newtype PostCouchdb_session = PostCouchdb_session
  { ok :: Boolean
  , name :: NullOrUndefined String
  , roles :: Array String
  }
derive instance genericPostCouchdb_session :: Generic PostCouchdb_session _
derive instance newtypePostCouchdb_session :: Newtype PostCouchdb_session _
instance respondablePostCouchdb_session :: Respondable PostCouchdb_session where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
