module Perspectives.Couchdb where

import Data.Foreign.Class (class Decode)
import Data.Foreign.Generic (defaultOptions, genericDecode)
import Data.Foreign.NullOrUndefined (NullOrUndefined)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax.Response (class Respondable, ResponseType(..))
import Prelude (($))

-----------------------------------------------------------
-- DOCUMENT
-----------------------------------------------------------

newtype PutCouchdbDocument = PutCouchdbDocument
  { ok :: Boolean
  , id :: String
  , rev :: String}

derive instance genericPutCouchdbDocument :: Generic PutCouchdbDocument _

derive instance newtypePutCouchdbDocument :: Newtype PutCouchdbDocument _

instance respondablePutCouchdbDocument :: Respondable PutCouchdbDocument where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

-----------------------------------------------------------
-- DB
-----------------------------------------------------------
newtype GetCouchdbAllDocs = GetCouchdbAllDocs
  { offset :: Int
  , rows :: Array DocReference
  , total_rows :: Int
  , update_seq :: NullOrUndefined Int
  }

derive instance genericCouchdbAllDocs :: Generic GetCouchdbAllDocs _
derive instance newtypeCouchdbAllDocs :: Newtype GetCouchdbAllDocs _

instance respondableGetCouchdbAllDocs :: Respondable GetCouchdbAllDocs where
  responseType = Tuple Nothing JSONResponse
  fromResponse = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

newtype DocReference = DocReference { id :: String, value :: Rev}

derive instance genericDocReference :: Generic DocReference _

instance decodeDocReference :: Decode DocReference where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}

newtype Rev = Rev { rev :: String}

derive instance genericRef :: Generic Rev _

instance decodeRev :: Decode Rev where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
